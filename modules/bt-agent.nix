{ config, pkgs, lib, ... }: {
  options.services.bt-agent = with lib; {
    enable = mkEnableOption "Bluetooth authentication agent";
    package = mkOption {
      default = pkgs.bluez-tools;
      type = types.package;
    };
    capability = mkOption {
      default = "DisplayYesNo";
      description = "Agent capability";
      type = types.enum [
        "DisplayOnly"
        "DisplayYesNo"
        "KeyboardOnly"
        "NoInputNoOutput"
      ];
    };
    pinFile = mkOption {
      default = null;
      type = types.nullOr types.path;
      description = "Path to the PIN's file";
    };
  };
  config = let
    cfg = config.services.bt-agent;
    args = [ "--capability=${cfg.capability}" ]
      ++ lib.optional (!isNull cfg.pinFile) "--pin=${cfg.pinFile}";
  in lib.mkIf cfg.enable {
    systemd.services.bt-agent = {
      path = [ cfg.package ];
      serviceConfig.Type = "forking";
      serviceConfig.KillSignal = "SIGKILL";
      script = "bt-agent -d ${lib.escapeShellArgs args}";
      wantedBy = [ "bluetooth.target" ];
      after = [ "bluetooth.service" ];
    };
  };
}
