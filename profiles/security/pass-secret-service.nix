{
  config,
  pkgs,
  lib,
  ...
}:
{
  services.dbus.packages = [ pkgs.pass-secret-service ];
  xdg.portal.extraPortals = lib.mkForce [ pkgs.pass-secret-service ];

  home-manager.users.balsoft = {
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
        Wants = [ "gpg-agent.service" ];
        After = Wants;
        PartOf = [ "graphical-session-pre.target" ];
      };
    };
  };
}
