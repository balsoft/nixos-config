{ pkgs, ... }: {
  home-manager.users.balsoft.home.packages = [ pkgs.josm ];
  persist.state.directories =
    [ "/home/balsoft/.local/share/JOSM" "/home/balsoft/.config/JOSM" ];
}
