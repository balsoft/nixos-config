{ config, pkgs, lib, ... }: {
  home-manager.users.balsoft.systemd.user.services = {
    simple-osd-pulseaudio = {
      Install.WantedBy = [ "default.target" ];
      Service = {
        ExecStart = "${pkgs.simple-osd.pulseaudio}/bin/simple-osd-pulseaudio";
      };
    };
    simple-osd-bluetooth = {
      Install.WantedBy = [ "default.target" ];
      Service = {
        ExecStart = "${pkgs.simple-osd.bluetooth}/bin/simple-osd-bluetooth";
      };
    };
  } // pkgs.lib.optionalAttrs (config.deviceSpecific.isLaptop) {
    simple-osd-battery = {
      Install.WantedBy = [ "default.target" ];
      Service = {
        ExecStart = "${pkgs.simple-osd.battery}/bin/simple-osd-battery";
      };
    };
    simple-osd-brightness = {
      Install.WantedBy = [ "default.target" ];
      Service = {
        ExecStart = "${pkgs.simple-osd.brightness}/bin/simple-osd-brightness";
      };
    };
  };
}
