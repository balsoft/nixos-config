{ pkgs, config, ... }:
let
  team_id = "E0440BK5L9J";
in
{
  home-manager.users.balsoft = {
    home.packages = [ pkgs.slk ];
    xdg.configFile."slk/config.toml" = {
      force = true;
      source = (pkgs.formats.toml { }).generate "slk.toml" {
        workspaces.${team_id} = {
          version_ts = "1793810257";
        };

        appearance = {
          theme = "custom";
          colored_usernames = true;
          compact = true;
          reply_avatars = true;
          timestamp_format = "15:04";
        };

        notifications = {
          enabled = true;
          on_mention = true;
          on_dm = true;
          notify_command = ''notify-send "$SLK_TITLE" "$SLK_BODY"'';
        };
      };
    };
    xdg.configFile."slk/themes/custom.toml".source = (pkgs.formats.toml { }).generate "custom.toml" {
      name = "custom";
      colors = with pkgs.my-lib.thmHash config.themes.colors; {
        primary = base05;
        accent = base0D;
        warning = base09;
        error = base08;
        background = base00;
        surface = base02;
        surface_dark = base00;
        text = base05;
        text_muted = base04;
        border = base01;
        sidebar_background = base00;
        sidebar_text = base05;
        sidebar_text_muted = base04;
        rail_background = base00;
        selection_background = base01;
        selection_foreground = base05;
        search_highlight_bg = base0C;
        search_highlight_fg = base05;
        compose_insert_bg = base01;
        selection_bg_focused = base01;
        selection_bg_unfocused = base01;
      };
    };
  };

  startupApplications = [ "${config.defaultApplications.term.cmd} -e ${pkgs.slk}/bin/slk" ];

  persist.state.homeDirectories = [ ".local/share/slk" ];

  systemd.tmpfiles.settings."50-slk"."/home/balsoft/.local/share/slk/tokens/${team_id}.json"."L+" = {
    user = "balsoft";
    group = "users";
    mode = "0700";
    argument = config.secrets.slk.decrypted;
  };

  secrets.slk = {
    owner = "balsoft:users";
    services = [ ];

    encrypted = "${config.environment.sessionVariables.PASSWORD_STORE_DIR}/slk/${team_id}.json.gpg";
  };
}
