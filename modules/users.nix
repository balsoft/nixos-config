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
    data = "[ -s .config/Yubico/u2f_keys ] || pamu2fcfg > .config/Yubico/u2f_keys";
    after = [ "linkGeneration" ];
    before = [ ];
  };
  home-manager.users.balsoft.home.sessionVariables.XDG_RUNTIME_DIR =
    "/run/user/1000";

  services.udev.extraRules = ''
    ACTION=="remove", ATTRS{idVendor}=="1050, RUN+="${
      pkgs.writeShellScript "lock-system"
      "/run/wrappers/bin/sudo -u balsoft 'swaylock -f -c ${
        builtins.substring 1 7 config.themes.colors.bg
      }'"
    }"'';
  security.pam.u2f = {
    control = "required";
    cue = true;
    enable = true;
  };
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
