{ config, pkgs, lib, ... }:
let
  simple-osd-daemon = name: {
    Install.WantedBy = [ "default.target" ];
    Service = {
      ExecStart = "${pkgs.simple-osd.${name}}/bin/simple-osd-${name}";
      Restart = "always";
      After = [ "mako.service" ];
      Wants = [ "mako.service" ];
    };
  };
  inherit (pkgs.my-lib) genIni;
  daemons = names:
    builtins.listToAttrs (builtins.map (name: {
      name = "simple-osd-${name}";
      value = simple-osd-daemon name;
    }) names);
in {
  home-manager.users.balsoft = {
    systemd.user.services = daemons [ "pulseaudio" "mpris" "bluetooth" ]
      // pkgs.lib.optionalAttrs (config.deviceSpecific.isLaptop)
      (daemons [ "battery" "brightness" ]);
    xdg.configFile = {
      "simple-osd/common".text =
        genIni { progressbar.length = 25; notification."default timeout" = 3; };
      "simple-osd/mpris".text =
        genIni { default."notification display time" = 3; };
    };
  };
}
