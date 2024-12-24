{ config, pkgs, ... }: {
  defaultApplications = {
    monitor = {
      cmd =
        "${config.defaultApplications.term.cmd} -T btop -e ${pkgs.btop}/bin/btop";
      desktop = "alacritty";
    };
  };
  home-manager.users.balsoft = {
    programs.btop = {
      enable = true;
      settings = {
        vim_keys = true;
        proc_gradient = false;
        color_theme = "Default";
        update_ms = 500;
        presets = "cpu:0:default,proc:0:default cpu:0:default,mem:0:default";
      };
    };
  };
}
