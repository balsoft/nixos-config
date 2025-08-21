{ pkgs, lib, config, ... }:
{
  home-manager.users.balsoft = {
    home.packages = [ pkgs.kdePackages.gwenview ];
    xdg.configFile."gwenviewrc".text = pkgs.my-lib.genIni {
      General = {
        HistoryEnabled = false;
        UrlNavigatorIsEditable = true;
      };
      ImageView.EnlargeSmallerImages = true;
      ThumbnailView.Sorting = "Sorting::Rating";
    };
  };

  defaultApplications.image_viewer = {
    desktop = "org.kde.gwenview";
    cmd = "${pkgs.gwenview}/bin/gwenview";
  };

}
