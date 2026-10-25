{ config, pkgs, lib, ... }: {

  secrets.nextcloud = {
    encrypted = "${config.secretsConfig.password-store}/nextcloud.balsoft.eu/balsoft.gpg";
    services = [ "home-manager-balsoft" ];
    owner = "balsoft:users";
  };

  home-manager.users.balsoft = {
    programs.rclone = {
      enable = true;
      # remotes.nextcloud = {
      #   config = {
      #     type = "webdav";
      #     url = "https://nextcloud.balsoft.eu/remote.php/dav/files/balsoft";
      #     vendor = "nextcloud";
      #     user = "balsoft";
      #   };
      #   secrets = {
      #     pass = config.secrets.nextcloud.decrypted;
      #   };
      #   mounts."" = {
      #     enable = true;
      #     mountPoint = "/home/balsoft/cloud/nextcloud.balsoft.eu";
      #     options = {
      #       vfs-cache-mode = "full";
      #     };
      #   };
      # };
    };
  };
}
