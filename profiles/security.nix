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
      "lp"
      "scanner"
    ];
    description = "Александр Бантьев";
    uid = 1000;
    password = "";
  };

  systemd.services."user@" = { serviceConfig = { Restart = "always"; }; };

  home-manager.users.balsoft = {
    systemd.user.services.polkit-agent = {
      Unit = {
        Description = "Run polkit authentication agent";
        X-RestartIfChanged = true;
      };

      Install.WantedBy = [ "sway-session.target" ];

      Service = { ExecStart = "${pkgs.mate.mate-polkit}/libexec/polkit-mate-authentication-agent-1"; };
    };
    home.activation.yubi = {
      data = ''
        mkdir -p .config/Yubico
        [ -f /home/balsoft/.config/Yubico/u2f_keys ] || (pamu2fcfg > /home/balsoft/.config/Yubico/u2f_keys)
      '';
      after = [ "linkGeneration" ];
      before = [ ];
    };
  };

  persist.state.directories = [ "/home/balsoft/.config/Yubico" ];

  services.udev.extraRules = ''
    ACTION=="remove", ATTRS{idVendor}=="1050", RUN+="${pkgs.systemd}/bin/loginctl lock-sessions"
  '';

  services.getty.autologinUser = "balsoft";

  environment.loginShellInit = lib.mkBefore ''
    [[ "$(tty)" == /dev/tty? ]] && sudo /run/current-system/sw/bin/lock this
  '';

  security.pam.u2f = {
    control = "sufficient";
    cue = true;
    enable = true;
  };

  environment.systemPackages = [
    (pkgs.writeShellScriptBin "lock" ''
      set -euo pipefail
      if [[ "$1" == this ]]
        then args="-s"
        else args="-san"
      fi
      ${
        lib.optionalString (config.deviceSpecific.isLaptop)
        ''USER=balsoft ${pkgs.vlock}/bin/vlock "$args"''
      }
    '')
  ];

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
      balsoft ALL = (root) NOPASSWD: /run/current-system/sw/bin/lock
      balsoft ALL = (root) NOPASSWD: /run/current-system/sw/bin/lock this
      balsoft ALL = (root) NOPASSWD: ${pkgs.light}/bin/light -A 5
      balsoft ALL = (root) NOPASSWD: ${pkgs.light}/bin/light -U 5
    '';
  };
  home-manager.useUserPackages = true;
}
