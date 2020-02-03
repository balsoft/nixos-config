{ pkgs, lib, config, ... }: {
  programs.sway.enable = true;
  users.users.balsoft.extraGroups = [ "sway" ];
  systemd.coredump.enable = true;
  programs.ssh.askPassword = "${pkgs.plasma5.ksshaskpass}/bin/ksshaskpass";
  environment.sessionVariables = {
    EDITOR = config.defaultApplications.editor.cmd;
    VISUAL = config.defaultApplications.editor.cmd;
    LESS = "-asrRix8";
    XDG_SESSION_TYPE = "wayland";
    XKB_DEFAULT_LAYOUT = "us,ru";
    XKB_DEFAULT_OPTIONS = "grp:lctrl_toggle,grp_led:caps,ctrl:nocaps";
    QT_QPA_PLATFORM = "wayland";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
    NIX_AUTO_RUN = "1";
  };
  services.atd.enable = true;
  home-manager.users.balsoft = {
    xdg.enable = true;

    home.activation."mimeapps-remove" = {
      before = [ "linkGeneration" ];
      after = [ ];
      data = "rm -f /home/balsoft/.config/mimeapps.list";
    };
    programs.gpg.enable = true;
    services.gpg-agent = {
      enable = true;
      extraConfig = ''
        pinentry-program ${pkgs.pinentry}/bin/pinentry
        allow-emacs-pinentry
        allow-loopback-pinentry
      '';
    };
    services.udiskie.enable = true;
    programs.git = {
      enable = true;
      userEmail = "balsoft@balsoft.ru";
      userName = "Alexander Bantyev";
      extraConfig.pull.rebase = true;
      signing = {
        signByDefault = true;
        key = "687558B21E04FE92B255BED0E081FF12ADCB4AD5";
      };
    };
    news.display = "silent";
    programs.command-not-found = {
      enable = true;
      dbPath = ../../misc/programs.sqlite;
    };
    home.keyboard = {
      options = [ "grp:win_space_toggle,grp_led:caps,ctrl:nocaps" ];
      layout = "us,ru";
    };
    home.file.".icons/default".source =
      "${pkgs.breeze-qt5}/share/icons/breeze_cursors";
    systemd.user.startServices = true;
  };
}
