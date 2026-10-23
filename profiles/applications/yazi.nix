{ pkgs, ... }:
{
  home-manager.users.balsoft = {
    programs.yazi = {
      enable = true;
      enableZshIntegration = true;

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
