{
  config,
  pkgs,
  lib,
  ...
}:
{
  services.dbus.packages = [ pkgs.pass-secret-service ];

  home-manager.users.balsoft = {
    xdg.portal = {
      extraPortals = [ pkgs.pass-secret-service ];
      config.common."org.freedesktop.impl.portal.Secrets" = [ "pass-secret-service" ];
    };

    services.pass-secret-service.enable = true;

    systemd.user.services.pass-secret-service = {
      Service = {
        Type = "dbus";
        Environment = [
          "GPG_TTY=/dev/tty1"
          "DISPLAY=:0"
        ];
        BusName = "org.freedesktop.secrets";
      };
      Unit = rec {
        Requires = [
          "gpg-agent.service"
          "activate-secrets.service"
        ];
        After = Requires;
        PartOf = [ "graphical-session-pre.target" ];
      };
    };
  };
}
