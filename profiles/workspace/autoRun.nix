{ config, pkgs, lib, ... }: {
  home-manager.users.balsoft = {
    programs.nix-index.enable = true;
  };

  environment.sessionVariables = {
    NIX_AUTO_RUN = "1";
  };
}
