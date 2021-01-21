{ pkgs, config, lib, ... }:

with rec { inherit (config) device deviceSpecific; };
with deviceSpecific; {
  hardware.enableRedistributableFirmware = true; # For some unfree drivers

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true; # For steam
  hardware.opengl.package = pkgs.mesa_drivers;

  hardware.bluetooth.enable = true;
  hardware.bluetooth.package = pkgs.bluezFull;

  hardware.sane.enable = true;

  services.saned.enable = true;

  services.logind.lidSwitchExternalPower = "ignore";

  services.logind.extraConfig = "HandlePowerKey=suspend";

  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull.overrideAttrs (oa: {
      patches = [
        (pkgs.fetchurl {
          url =
            "https://gitlab.freedesktop.org/pulseaudio/pulseaudio/-/merge_requests/239.diff";
          sha256 = "07qrpqwvn9sr87z2kn1yaza5bs9ivywd7sl194zwphlq94xrlzdf";
        })
      ];
    });
    support32Bit = true;
    extraConfig = ''
      load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1
      load-module module-bluetooth-policy auto_switch=2
    '';
  };
}
