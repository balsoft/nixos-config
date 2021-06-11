{ config, pkgs, lib, ... }: {
  home-manager.users.balsoft = {
    programs.direnv.enable = true;
    programs.direnv.enableNixDirenvIntegration = true;
  };
  persist.state.directories =
    [ "/home/balsoft/.local/share/direnv" ];
}
