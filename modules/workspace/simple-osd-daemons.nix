{ config, pkgs, lib, ... }:
let
  simple-osd-daemon = name: {
    Install.WantedBy = [ "default.target" ];
    Service = {
      ExecStart = "${pkgs.simple-osd.${name}}/bin/simple-osd-${name}";
      Restart = "always";
    };
  };
  inherit (import ../../support.nix { inherit lib config; }) genIni;
  daemons = names:
    builtins.listToAttrs (builtins.map (name: {
      name = "simple-osd-${name}";
      value = simple-osd-daemon name;
    }) names);
in {
  home-manager.users.balsoft = {
    systemd.user.services = daemons [ "pulseaudio" "mpris" ]
      // pkgs.lib.optionalAttrs (config.deviceSpecific.isLaptop)
      (daemons [ "battery" "brightness" ]);
    xdg.configFile."simple-osd/common".text =
      genIni { progressbar.length = 25; };
    xdg.configFile."simple-osd/battery" =
      lib.mkIf (config.deviceSpecific.isLaptop) ({
        text = genIni {
          default = {
            "refresh interval" = 1;
            "show battery charge" = true;
          };
          threshold = {
            low = "20%";
            critical = "10%";
          };
        };
      });
  };
}
