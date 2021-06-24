{ config, pkgs, lib, inputs, ... }: {
  home-manager.users.balsoft = {
    programs.direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
        enableFlakes = true;
        # package = inputs.nix-direnv.defaultPackage.${pkgs.system};
      };
    };
  };
  persist.state.directories =
    [ "/home/balsoft/.local/share/direnv" ];
}
