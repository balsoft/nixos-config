{ config, pkgs, lib, ... }: {
  home-manager.users.balsoft = {
    programs.direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
        enableFlakes = true;
      };
    };
  };
  persist.state.directories =
    [ "/home/balsoft/.local/share/direnv" ];
}
