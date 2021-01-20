{ pkgs, lib, config, inputs, ... }: {
  programs.sway.enable = true;
  users.users.balsoft.extraGroups = [ "sway" ];
  systemd.coredump.enable = true;
  environment.sessionVariables = config.home-manager.users.balsoft.home.sessionVariables // {
    EDITOR = config.defaultApplications.editor.cmd;
    VISUAL = config.defaultApplications.editor.cmd;
    LESS = "-asrRix8";
    XDG_SESSION_TYPE = "wayland";
    QT_QPA_PLATFORM = "wayland";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
    NIX_AUTO_RUN = "1";
    XCURSOR_PATH = lib.mkForce "/home/balsoft/.icons";
  };

  services.atd.enable = true;

  containers.fhs-compat.config = {...}: {
    imports = [ inputs.nixos-fhs-compat.nixosModules.combined ];

    environment.fhs.enable = true;
    environment.fhs.linkLibs = true;
    environment.lsb.enable = true;

    users.users.balsoft = {
      isNormalUser = true;
      password = "";
    };
  };

  home-manager.users.balsoft = {
    xdg.enable = true;

    home.activation."mimeapps-remove" = {
      before = [ "linkGeneration" ];
      after = [ ];
      data = "rm -f /home/balsoft/.config/mimeapps.list";
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
    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      pinentryFlavor = "gtk2";
    };
    programs.gpg.enable = true;
    news.display = "silent";
    programs.command-not-found = {
      enable = true;
      dbPath = ../../misc/programs.sqlite;
    };
    home.keyboard = {
      options = [ "grp:win_space_toggle,grp_led:caps,ctrl:nocaps" ];
      layout = "us,ru";
    };
    systemd.user.startServices = true;
    services.kdeconnect.enable = true;

    xsession.pointerCursor = {
      package = pkgs.breeze-qt5;
      name = "Breeze";
    };

    home.file.".icons/default".source = "${pkgs.breeze-qt5}/share/icons/breeze_cursors";
  };

}
