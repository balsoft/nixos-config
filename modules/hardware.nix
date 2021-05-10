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

  hardware.sane.enable = true;

  services.saned.enable = true;

  services.logind.lidSwitchExternalPower = "ignore";

  services.logind.extraConfig = "HandlePowerKey=suspend";

  # sound.enable = true;

  hardware.pulseaudio.enable = false;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
    media-session.config.bluez-monitor = {
      properties = { };
      rules = [
        {
          actions = {
            update-props = {
              "bluez5.auto-connect" = [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
              "bluez5.hw-volume" =
                [ "hfp_ag" "hsp_ag" "a2dp_source" "a2dp_sink" ];
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
