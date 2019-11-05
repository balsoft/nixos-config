{ pkgs, config, lib, ...}:
{
  
  home-manager.users.balsoft = {
    home.file.".yt.yaml".text = ''
      yt-token: ${config.secrets.yt-utilities.token}
      user: ${config.secrets.yt-utilities.user}
      slack-token: ${config.secrets.slack-term}
      from:
        org: "/home/balsoft/cloud/Google Drive/serokell.org"
        full-file: true
      since: 1997-11-19
      severity: Debug
    '';
    home.packages = [ pkgs.yt-utilities ];
  };
}
