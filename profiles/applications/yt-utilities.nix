{ pkgs, config, lib, inputs, ... }: {
  home-manager.users.balsoft = {
    home.packages = [ inputs.yt-utilities.defaultPackage.x86_64-linux ];
    home.activation.yt-config = "$DRY_RUN_CMD ln -sf $VERBOSE_ARG ${config.secrets-envsubst.yt} $HOME/.yt.yaml";
  };
  secrets-envsubst.yt = {
    secrets = [ "user" "token" ];
    owner = "balsoft:users";
    template = builtins.toJSON {
      yt-token = "$token";
      user = "$user";
      from = {
        org = "/home/balsoft/Documents/serokell.org";
        full-file = true;
      };
      since = "1997-11-19";
      severity = "Info";
    };
  };
}
