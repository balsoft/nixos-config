{ config, pkgs, lib, ... }: {
  security.apparmor.enable = true;
  programs.firejail.enable = true;
  users.mutableUsers = false;
  users.users.balsoft = {
    isNormalUser = true;
    extraGroups = [
      "sudo"
      "wheel"
      "networkmanager"
      "disk"
      "dbus"
      "audio"
      "docker"
      "sound"
      "pulse"
      "adbusers"
      "input"
      "libvirtd"
      "vboxusers"
      "wireshark"
    ];
    description = "Александр Бантьев";
    uid = 1000;
    password = "";
  };

  systemd.services."user@" = { serviceConfig = { Restart = "always"; }; };

  home-manager.users.balsoft.home.activation.yubi = {
    data =
      "[ -s /home/balsoft/.config/Yubico/u2f_keys ] || (pamu2fcfg > /home/balsoft/.config/Yubico/u2f_keys)";
    after = [ "linkGeneration" ];
    before = [ ];
  };
  home-manager.users.balsoft.home.sessionVariables.XDG_RUNTIME_DIR =
    "/run/user/1000";

  services.udev.extraRules = ''
    ACTION=="remove", ATTRS{idVendor}=="1050", RUN+="${pkgs.systemd}/bin/systemctl start swaylock"
    ACTION=="remove", ATTRS{idVendor}=="1050", RUN+="${pkgs.systemd}/bin/systemctl restart swaylock"  
  '';

  systemd.services.swaylock = {
    description = "Lock the screen";
    serviceConfig.User = "balsoft";
    environment.XDG_RUNTIME_DIR = "/run/user/1000";
    path = [pkgs.swaylock];
    script = "swaylock -c ${builtins.substring 1 7 config.themes.colors.bg}";
  };

  security.pam.u2f = {
    control = "sufficient";
    cue = true;
    enable = true;
  };

  security.pam.services = builtins.listToAttrs (builtins.map (name: {
    inherit name;
    value = { unixAuth = false; };
  }) [
    "chpasswd"
    "chsh"
    "groupadd"
    "groupdel"
    "groupmems"
    "groupmod"
    "i3lock"
    "i3lock-color"
    "login"
    "passwd"
    "polkit-1"
    "runuser"
    "runuser-l"
    "su"
    "sudo"
    "swaylock"
    "systemd-user"
    "useradd"
    "userdel"
    "usermod"
    "vlock"
    "xlock"
    "xscreensaver"
  ]);

  security.sudo = {
    enable = true;
    extraConfig = ''
      balsoft ALL = (root) NOPASSWD: ${pkgs.light}/bin/light -A 5
      balsoft ALL = (root) NOPASSWD: ${pkgs.light}/bin/light -U 5
    '';
  };
  nix.requireSignedBinaryCaches = false;
  home-manager.useUserPackages = true;
}
