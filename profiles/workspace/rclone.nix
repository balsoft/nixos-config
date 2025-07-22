{ config, pkgs, lib, ... }: {

  secrets.nextcloud = {
    encrypted = "${config.secretsConfig.password-store}/nextcloud.balsoft.ru/balsoft.gpg";
    services = [ "home-manager-balsoft" ];
    owner = "balsoft:users";
  };

  home-manager.users.balsoft = {
    programs.rclone = {
      enable = true;
      remotes.nextcloud = {
        config = {
          type = "webdav";
          url = "https://nextcloud.balsoft.ru/remote.php/dav/files/balsoft";
          vendor = "nextcloud";
          user = "balsoft";
        };
        secrets = {
          pass = config.secrets.nextcloud.decrypted;
        };
        mounts."" = {
          enable = true;
          mountPoint = "/home/balsoft/nextcloud.balsoft.ru";
          options = {
            vfs-cache-mode = "full";
          };
        };
      };
    };
  };
}
