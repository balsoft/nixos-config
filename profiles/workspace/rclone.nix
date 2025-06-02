{ config, pkgs, lib, ... }: {
  secrets-envsubst.rclone = {
    owner = "balsoft:users";
    secrets = [ "nextcloud" ];
    services = [ "home-manager-balsoft" ];
    template = lib.generators.toINI {} {
      nextcloud = {
        type = "webdav";
        url = "https://nextcloud.balsoft.ru/remote.php/dav/files/balsoft";
        vendor = "nextcloud";
        user = "balsoft";
        pass = "$nextcloud";
      };
    };
  };

  home-manager.users.balsoft.home = {
    activation.rclone = ''
      mkdir -p $XDG_CONFIG_HOME/rclone
      ln -sf ${config.secrets-envsubst.rclone.substituted} $XDG_CONFIG_HOME/rclone/rclone.conf
    '';
    packages = [ pkgs.rclone ];
  };
}
