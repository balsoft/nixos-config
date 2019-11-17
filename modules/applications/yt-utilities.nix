{ pkgs, config, lib, ... }: {
  home-manager.users.balsoft = lib.mkIf (config.deviceSpecific.goodMachine) {
    home.file.".yt.yaml".text = builtins.toJSON {
      yt-token = config.secrets.yt-utilities.token;
      user = config.secrets.yt-utilities.user;
      from = {
        org = "/home/balsoft/cloud/Google Drive/serokell.org";
        full-file = true;
      };
      since = "1997-11-19";
      severity = "Debug";
    };
    home.packages = [ pkgs.yt-utilities ];
  };
}
