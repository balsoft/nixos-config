{ config, pkgs, lib, inputs, ... }: {
  home-manager.users.balsoft = {
    programs.direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
      };
    };
  };
  persist.state.directories =
    [ "/home/balsoft/.local/share/direnv" ];
}
