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
    xdg.configFile."gtk-3.0/bookmarks".text = builtins.concatStringsSep "\n" ([
      "file:///home/balsoft/projects Projects"
      "davs://nextcloud.balsoft.ru/remote.php/dav/files/balsoft nextcloud.balsoft.ru"
    ] ++ map (machine: "sftp://${machine}/home/balsoft ${machine}")
      (builtins.attrNames config.devices));
    xdg.userDirs.enable = true;
  };
}
