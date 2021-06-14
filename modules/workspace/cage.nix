{ config, pkgs, lib, ... }: {
  services.cage = {
    enable = true;
    program = config.defaultApplications.term.cmd;
    user = "balsoft";
  };
}
