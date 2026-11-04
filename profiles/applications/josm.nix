{ pkgs, ... }: {
  home-manager.users.balsoft.home.packages = [ pkgs.josm ];
  persist.state.homeDirectories =
    [ ".local/share/JOSM" ".config/JOSM" ];
}
