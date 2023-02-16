{ config, pkgs, lib, ... }: {
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
  };


  services.getty.autologinUser = "balsoft";

  home-manager.useUserPackages = true;
}
