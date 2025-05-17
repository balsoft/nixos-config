{ pkgs, config, ... }:
{
  home-manager.users.balsoft = {
    systemd.user.services.mako = {
      Service = {
        ExecStart = "${pkgs.mako}/bin/mako";
        Environment = [
          "PATH=${
            pkgs.lib.makeBinPath [
              pkgs.bash
              pkgs.mpv
            ]
          }"
        ];
      };
      Install = {
        After = [ "sway-session.target" ];
        WantedBy = [ "sway-session.target" ];
      };
    };
    services.mako = with (pkgs.my-lib.thmHash config.themes.colors); {
      enable = true;
      settings =
        let
          play = sound: "mpv ${pkgs.sound-theme-freedesktop}/share/sounds/freedesktop/stereo/${sound}.oga";
        in
        {
          "urgency=high".border-color = "${base09}AA";
          "urgency=critical" = {
            border-color = "${base09}AA";
            on-notify = "exec ${play "message"}";
          };
          "app-name=yubikey-touch-detector".on-notify = "exec ${play "service-login"}";
          "app-name=command_complete summary~=\"✘.*\"".on-notify = "exec ${play "dialog-warning"}";
          "app-name=command_complete summary~=\"✓.*\"".on-notify = "exec ${play "bell"}";
          "category=osd".on-notify = "none";
          "mode=do-not-disturb".invisible = 1;
          "mode=do-not-disturb summary=\"Do not disturb: on\"".invisible = 0;
          "mode=concentrate".invisible = 1;
          "mode=concentrate urgency=critical".invisible = 0;
          "mode=concentrate summary=\"Concentrate mode: on\"".invisible = 0;
          layer = "overlay";
          font = with config.themes.fonts; "${main.family} ${toString main.size}";
          width = 500;
          height = 160;
          default-timeout = 10000;
          max-visible = 10;
          background-color = "${base00}AA";
          text-color = base05;
          border-color = "${base0D}AA";
          progress-color = "over ${base0B}";
          icon-path = "${pkgs.kdePackages.breeze-icons}/share/icons/breeze-dark";
          max-icon-size = 24;
        };
    };
  };
}
