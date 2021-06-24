{ config, pkgs, lib, ... }: {
  home-manager.users.balsoft.home.packages = [ pkgs.nheko ];
  defaultApplications.matrix = {
    cmd = "${pkgs.nheko}/bin/nheko";
    desktop = "nheko";
  };
  persist.state.directories =
    [ "/home/balsoft/.local/share/nheko" "/home/balsoft/.config/nheko" ];
}
