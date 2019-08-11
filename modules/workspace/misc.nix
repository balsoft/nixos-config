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
    XKB_DEFAULT_OPTIONS = "grp:caps_toggle,grp_led:caps";
    QT_QPA_PLATFORM = "wayland";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
    NIX_AUTO_RUN = "1";
  };
  services.atd.enable = true;
  home-manager.users.balsoft = {
    xdg.enable = true;

    services.udiskie.enable = true;
    programs.git = {
      enable = true;
      userEmail = "balsoft@yandex.ru";
      userName = "Alexander Bantyev";
    };
    news.display = "silent";
    programs.command-not-found = {
      enable = true;
      dbPath = ../../imports/programs.sqlite;
    };
    home.keyboard = {
      options = [ "grp:caps_toggle,grp_led:caps" ];
      layout = "us,ru";
    };
    home.file.".icons/default".source =
      "${pkgs.breeze-qt5}/share/icons/breeze_cursors";
    systemd.user.startServices = true;
  };
}
