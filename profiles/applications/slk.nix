{ pkgs, config, ... }:
let
  team_id = "E0440BK5L9J";
in
{
  home-manager.users.balsoft = {
    home.packages = [ pkgs.slk ];
    xdg.configFile."slk/config.toml" = {
      force = true;
      text = with pkgs.lib.generators; toINI {
        mkKeyValue = mkKeyValueDefault {
          mkValueString = value: "\"${pkgs.lib.escape [ "\"" ] value}\"";
        } "=";
      } {
        "workspaces.${team_id}" = {
          theme = "ANSI Dark";
          version_ts = "1793810257";
        };

        theme = with pkgs.my-lib.thmHash config.themes.colors; {
          surface = base01;
          surface_dark = base00;
          text_muted = base04;
        };
      };
    };
  };

  startupApplications = [ "${config.defaultApplications.term.cmd} -e ${pkgs.slk}/bin/slk" ];

  persist.state.homeDirectories = [ ".local/state/slk" ];

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
