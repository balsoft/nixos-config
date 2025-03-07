{ pkgs, config, ... }: {
  home-manager.users.balsoft = {
    systemd.user.services.mako = {
      Service = {
        ExecStart = "${pkgs.mako}/bin/mako";
        Environment =
          [ "PATH=${pkgs.lib.makeBinPath [ pkgs.bash pkgs.mpv ]}" ];
      };
      Install = {
        After = [ "sway-session.target" ];
        WantedBy = [ "sway-session.target" ];
      };
    };
    services.mako = with (pkgs.my-lib.thmHash config.themes.colors); {
      enable = true;
      layer = "overlay";
      font = with config.themes.fonts; "${main.family} ${toString main.size}";
      width = 500;
      height = 160;
      defaultTimeout = 10000;
      maxVisible = 10;
      backgroundColor = "${base00}AA";
      textColor = base05;
      borderColor = "${base0D}AA";
      progressColor = "over ${base0B}";
      iconPath = "${pkgs.kdePackages.breeze-icons}/share/icons/breeze-dark";
      maxIconSize = 24;
      extraConfig = let
        play = sound:
          "mpv ${pkgs.sound-theme-freedesktop}/share/sounds/freedesktop/stereo/${sound}.oga";
      in ''
        [urgency=high]
        border-color=${base09}AA
        [urgency=critical]
        border-color=${base09}AA
        on-notify=exec ${play "message"}
        [app-name=yubikey-touch-detector]
        on-notify=exec ${play "service-login"}
        [app-name=command_complete summary~="✘.*"]
        on-notify=exec ${play "dialog-warning"}
        [app-name=command_complete summary~="✓.*"]
        on-notify=exec ${play "bell"}
        [category=osd]
        on-notify=none
        [mode=do-not-disturb]
        invisible=1
        [mode=do-not-disturb summary="Do not disturb: on"]
        invisible=0
        [mode=concentrate]
        invisible=1
        [mode=concentrate urgency=critical]
        invisible=0
        [mode=concentrate summary="Concentrate mode: on"]
        invisible=0
      '';
    };
  };
}
