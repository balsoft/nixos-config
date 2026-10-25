{
  pkgs,
  config,
  lib,
  ...
}:
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
    xdg.configFile."org.freedesktop.FileManager1.common/config".text =
      "cmd=${pkgs.org-freedesktop-filemanager1-common}/share/org.freedesktop.FileManager1.common/yazi-wrapper.sh";

    xdg.configFile."xdg-desktop-portal-termfilechooser/config".text = pkgs.my-lib.genIni {
      filechooser = {
        cmd = "${pkgs.xdg-desktop-portal-termfilechooser}/share/xdg-desktop-portal-termfilechooser/yazi-wrapper.sh";
        default_dir = "$HOME";
        env = [ "TERMCMD=xdg-terminal-exec" ];
        open_mode = "suggested";
        save_mode = "last";
      };
    };
    xdg.portal = {
      enable = true;
      extraPortals = lib.mkForce [ pkgs.xdg-desktop-portal-termfilechooser ];

      config = {
        common = {
          default = "wlr";
          "org.freedesktop.impl.portal.FileChooser" = [ "termfilechooser" ];
          "org.freedesktop.impl.portal.Secrets" = [ "pass-secret-service" ];
        };
      };
    };

    systemd.user.services.org-freedesktop-filemanager1-common = {
      Unit = {
        Description = "Run a FileManager1 interface to launch ${config.defaultApplications.fm.desktop}";
        X-RestartIfChanged = true;
      };

      Install.WantedBy = [ "sway-session.target" ];

      Service = {
        Environment = [ "TERMCMD=xdg-terminal-exec" ];
        ExecStart = "${pkgs.org-freedesktop-filemanager1-common}/libexec/file_manager_dbus";
      };
    };

    programs.yazi = {
      enable = true;
      shellWrapperName = "y";
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
