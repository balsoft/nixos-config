{ pkgs, config, lib, ... }: {
  hardware.pulseaudio.enable = lib.mkForce false;

  security.rtkit.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
    config.pipewire-pulse = {
      "context.modules" = [
        {
          "args" = { };
          "flags" = [ "ifexists" "nofail" ];
          "name" = "libpipewire-module-rtkit";
        }
        { "name" = "libpipewire-module-protocol-native"; }
        { "name" = "libpipewire-module-client-node"; }
        { "name" = "libpipewire-module-adapter"; }
        { "name" = "libpipewire-module-metadata"; }
        {
          "args" = {
            "server.address" = [ "unix:native" "tcp:4713" ];
            "vm.overrides" = { "pulse.min.quantum" = "1024/48000"; };
          };
          "name" = "libpipewire-module-protocol-pulse";
        }
      ];
      "context.properties" = { };
      "context.spa-libs" = {
        "audio.convert.*" = "audioconvert/libspa-audioconvert";
        "support.*" = "support/libspa-support";
      };
      "stream.properties" = { };
    };
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
              "bluez5.autoswitch-profile" = false;
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
