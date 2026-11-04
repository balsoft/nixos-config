{ config, pkgs, lib, ... }: {
  home-manager.users.balsoft = {
    programs.nix-index.enable = true;
  };

  persist.cache.homeDirectories = [ ".cache/nix-index" ];

  environment.sessionVariables = {
    NIX_AUTO_RUN = "1";
  };
}
