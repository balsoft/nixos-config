{ config, pkgs, lib, ... }: {
  services.gnome = {
    core-os-services.enable = true;
    core-utilities.enable = true;
    evolution-data-server.enable = true;
    sushi.enable = true;
    tracker.enable = true;
    tracker-miners.enable = true;
    gnome-settings-daemon.enable = true;
    glib-networking.enable = true;
    gnome-keyring.enable = true;
    gnome-online-accounts.enable = true;
    gnome-online-miners.enable = true;
  };

  services.gvfs.enable = true;
  services.geoclue2.enable = true;

  fileSystems = with config.persist;
    lib.mkIf enable (builtins.listToAttrs (map (name: {
      inherit name;
      value.options = [ "x-gvfs-hide" ];
    }) (state.directories ++ cache.directories ++ derivative.directories)));

  defaultApplications = {
    fm = {
      cmd = "${pkgs.gnome3.nautilus}/bin/nautilus";
      desktop = "org.gnome.Nautilus";
    };
    monitor = {
      cmd = "${pkgs.gnome3.gnome-system-monitor}/bin/gnome-system-monitor";
      desktop = "gnome-system-monitor";
    };
    archive = {
      cmd = "${pkgs.gnome3.file-roller}/bin/file-roller";
      desktop = "org.gnome.FileRoller";
    };
  };

  home-manager.users.balsoft = {
    home.activation.gnome = ''
      $DRY_RUN_CMD mkdir -p "$XDG_DATA_HOME/keyrings"
      $DRY_RUN_CMD ln -sf ${config.secrets-envsubst.gnome-keyring} "$XDG_DATA_HOME/keyrings/Default_keyring.keyring"
      echo "Default_keyring" > "$XDG_DATA_HOME/keyrings/default"
      $DRY_RUN_CMD mkdir -p "$XDG_CONFIG_HOME/goa-1.0"
      $DRY_RUN_CMD ln -sf ${
        ./accounts.conf
      } "$XDG_CONFIG_HOME/goa-1.0/accounts.conf"
      $DRY_RUN_CMD mkdir -p "$XDG_CONFIG_HOME/evolution/sources"
      $DRY_RUN_CMD ln -sf ${
        ./nextcloud.source
      } "$XDG_CONFIG_HOME/evolution/sources/nextcloud.source"
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

      "org/gnome/desktop/interface" = { cursor-theme = "default"; };

      "org/gnome/evince/default" = { inverted-colors = true; };

      "org/gnome/maps" = {
        night-mode = true;
        transportation-type = "car";
      };
    };
  };
  secrets-envsubst.gnome-keyring = {
    owner = "balsoft:users";
    secrets = [ "matrix_token" "matrix_password" "email" "nextcloud" ];
    template = builtins.readFile ./gnome-keyring-template.conf;
  };
}
