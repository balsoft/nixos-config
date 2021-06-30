{ pkgs, config, lib, inputs, ... }:

with rec { inherit (config) device deviceSpecific; };
with deviceSpecific; {
  hardware.enableRedistributableFirmware = true; # For some unfree drivers

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true; # For steam
  hardware.opengl.package = pkgs.mesa_drivers;

  hardware.bluetooth.enable = true;
  hardware.bluetooth.package = pkgs.bluezFull;

  persist.state.directories = [ "/var/lib/bluetooth" ];


  systemd.services.systemd-udev-settle.enable = false;

  services.upower = {
    enable = true;
  };


  services.logind.lidSwitchExternalPower = "ignore";

  services.logind.extraConfig = "HandlePowerKey=suspend";

  # sound.enable = true;

  hardware.pulseaudio.enable = false;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
    media-session.config.bluez-monitor = {
      properties = {
        "bluez5.codecs" = [ "sbc" "aac" "ldac" "aptx" "aptx_hd" ];
        "bluez5.mdbc-support" = true;
      };
      rules = [
        {
          actions = {
            update-props = {
              "bluez5.auto-connect" = [ "hsp_hs" "hfp_hf" "a2dp_sink" ];
              "bluez5.hw-volume" =
                [ "hsp_ag" "hfp_ag" "a2dp_source" "a2dp_sink" ];
            };
          };
          matches = [{ "device.name" = "~bluez_card.*"; }];
        }
        {
          actions = { update-props = { "node.pause-on-idle" = false; }; };
          matches = [
            { "node.name" = "~bluez_input.*"; }
            { "node.name" = "~bluez_output.*"; }
          ];
        }
      ];
    };
  };
}
