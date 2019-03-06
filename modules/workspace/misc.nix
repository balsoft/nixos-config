{pkgs, lib, config, ...}:
{
  programs.sway-beta.enable = true;
  users.users.balsoft.extraGroups = [ "sway" ];

  programs.ssh.askPassword = "${pkgs.plasma5.ksshaskpass}/bin/ksshaskpass";
  environment.sessionVariables = {
    EDITOR = config.defaultApplications.editor.cmd;
    VISUAL = config.defaultApplications.editor.cmd;
    LESS = "-asrRix8";
    XDG_SESSION_TYPE = "wayland";
    XKB_DEFAULT_LAYOUT = "us,ru";
    XKB_DEFAULT_OPTIONS = "grp:caps_toggle,grp_led:caps";
  };
  home-manager.users.balsoft =
  {
    xdg.enable = true;
    
    #services.udiskie.enable = true;
    programs.git =
    {
      enable = true;
      userEmail = "balsoft@yandex.ru";
      userName = "Александр Бантьев";
    };
    news.display = "silent";
    programs.command-not-found.enable = true;
    home.keyboard =
    {
      options = ["grp:caps_toggle,grp_led:caps"];
      layout = "us,ru";
    };
    
    home.file.".icons/default".source = "${pkgs.breeze-qt5}/share/icons/breeze_cursors";
    systemd.user.startServices = true;
  };
}
