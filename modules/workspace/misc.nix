{ pkgs, lib, config, ... }: {

  programs.ssh.askPassword = "${pkgs.plasma5.ksshaskpass}/bin/ksshaskpass";
  environment.sessionVariables = {
    EDITOR = config.defaultApplications.editor.cmd;
    VISUAL = config.defaultApplications.editor.cmd;
    LESS = "-asrRix8";
    NIX_AUTO_RUN = "1";
  };
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
