{ config, pkgs, lib, ... }: {
  environment.systemPackages = [ pkgs.pass-secret-service ];

  environment.gnome.excludePackages = [ pkgs.gnome-console pkgs.gnome.evince pkgs.gnome.eog ];

  services.dbus.packages = [ pkgs.pass-secret-service ];
  xdg.portal.extraPortals = [ pkgs.pass-secret-service ];

  services.gvfs.enable = true;
  services.geoclue2.enable = true;

  fileSystems = with config.persist;
    lib.mkIf enable (builtins.listToAttrs (map (name: {
      inherit name;
      value.options = [ "x-gvfs-hide" ];
    }) (state.directories ++ cache.directories ++ derivative.directories)));

  defaultApplications = {
    monitor = {
      cmd = "${pkgs.gnome-system-monitor}/bin/gnome-system-monitor";
      desktop = "gnome-system-monitor";
    };
  };

  home-manager.users.balsoft = {

    home.activation.gnome = ''
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

      "org/gnome/evince/default" = { inverted-colors = true; };

      "org/gnome/maps" = {
        night-mode = true;
        transportation-type = "car";
      };
    };
  };
}
