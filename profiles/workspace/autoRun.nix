{ config, pkgs, lib, ... }: {
  home-manager.users.balsoft = {
    programs.nix-index.enable = true;
  };

  persist.cache.directories = [ "/home/balsoft/.cache/nix-index" ];

  environment.sessionVariables = {
    NIX_AUTO_RUN = "1";
  };
}
