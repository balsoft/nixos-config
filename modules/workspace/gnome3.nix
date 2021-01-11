{ config, pkgs, lib, ... }: {
  services.gnome3 = {
    core-os-services.enable = true;
    core-utilities.enable = true;
    sushi.enable = true;
    tracker.enable = true;
    tracker-miners.enable = true;
    gnome-settings-daemon.enable = true;
    glib-networking.enable = true;
  };
  services.gvfs.enable = true;
  services.geoclue2.enable = true;
  home-manager.users.balsoft = {
    xdg.userDirs.enable = true;
    home.activation.gnome-keyring = ''
      ln -sf ${config.secrets-envsubst.gnome-keyring} "$XDG_DATA_HOME/keyrings/Default_keyring.keyring"
      echo "Default_keyring" > "$XDG_DATA_HOME/keyrings/default"
    '';
    dconf.settings = {
      "org/gnome/nautilus/icon-view" = {
        captions = [ "size" "date_modified" "none" ];
      };
      "org/gnome/nautilus/list-view" = {
        default-column-order = [
          "name"
          "size"
          "type"
          "owner"
          "group"
          "permissions"
          "where"
          "date_modified"
          "date_modified_with_time"
          "date_accessed"
          "recency"
          "starred"
          "detailed_type"
        ];
        default-visible-columns = [ "name" "size" "date_modified" "starred" ];
      };
      "org/gnome/nautilus/preferences" = {
        default-folder-viewer = "list-view";
        executable-text-activation = "display";
        search-filter-time-type = "last_modified";
        search-view = "list-view";
        show-image-thumbnails = "always";
        thumbnail-limit = 10;
      };

      "org/gnome/evince/default" = {
        inverted-colors = true;
      };

      "org/gnome/maps" = {
        night-mode = true;
        transportation-type = "car";
      };
    };
  };
  secrets-envsubst.gnome-keyring = {
    owner = "balsoft:users";
    secrets = [ "matrix_token" "matrix_password" "email" "nextcloud" ];
    template = ''
      [keyring]
      display-name=Default keyring
      ctime=1609508068
      mtime=0
      lock-on-idle=false
      lock-after=false

      [5]
      item-type=0
      display-name=fractal-token
      secret=$matrix_token
      mtime=1610374847
      ctime=1610374847

      [5:attribute0]
      name=uid
      type=string
      value=@balsoft:balsoft.ru

      [3]
      item-type=0
      display-name=Geary IMAP password
      secret=$email
      mtime=1610307006
      ctime=1609572471

      [3:attribute0]
      name=host
      type=string
      value=balsoft.ru

      [3:attribute1]
      name=login
      type=string
      value=balsoft@balsoft.ru

      [3:attribute2]
      name=proto
      type=string
      value=IMAP

      [3:attribute3]
      name=xdg:schema
      type=string
      value=org.gnome.Geary

      [1]
      item-type=0
      display-name=GOA owncloud credentials for identity account_1609508064_4
      secret={'password': <'$nextcloud'>}
      mtime=1609508086
      ctime=1609508086

      [1:attribute0]
      name=goa-identity
      type=string
      value=owncloud:gen0:account_1609508064_4

      [1:attribute1]
      name=xdg:schema
      type=string
      value=org.gnome.OnlineAccounts

      [4]
      item-type=0
      display-name=fractal
      secret=$matrix_password
      mtime=1610374846
      ctime=1610374846

      [4:attribute0]
      name=identity
      type=string
      value=https://vector.im/

      [4:attribute1]
      name=server
      type=string
      value=https://balsoft.ru/

      [4:attribute2]
      name=username
      type=string
      value=balsoft
    '';
  };
}
