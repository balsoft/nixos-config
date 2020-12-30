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
  home-manager.users.balsoft = {
    xdg.userDirs.enable = true;
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
}
