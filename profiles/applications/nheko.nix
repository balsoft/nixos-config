{ config, pkgs, lib, ... }: {
  home-manager.users.balsoft.home.packages = [ pkgs.nheko ];
  defaultApplications.matrix = {
    cmd = "${pkgs.nheko}/bin/nheko";
    desktop = "nheko";
  };
  startupApplications = [
    "${pkgs.nheko}/bin/nheko"
  ];
  persist.state.homeDirectories =
    [ ".local/share/nheko" ".config/nheko" ];
}
