{ pkgs, config, ... }:
{
  defaultApplications = {
    fm = {
      cmd = "${config.defaultApplications.term.cmd} -e yazi";
      desktop = "yazi";
    };
    archive = {
      cmd = "${config.defaultApplications.term.cmd} -e yazi";
      desktop = "yazi";
    };
  };
  home-manager.users.balsoft = {
    programs.yazi = {
      enable = true;

      keymap = {
        mgr.prepend_keymap = [
          {
            on = "M";
            run = "plugin mount";
          }
        ];
      };

      plugins = {
        mount = pkgs.yaziPlugins.mount;
      };
    };
  };
}
