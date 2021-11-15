{ config, pkgs, ... }: {
  startupApplications = with config.defaultApplications; [
    browser.cmd
    mail.cmd
    matrix.cmd
  ];

  environment.sessionVariables = {
    EDITOR = config.defaultApplications.editor.cmd;
    VISUAL = config.defaultApplications.editor.cmd;
  };

  home-manager.users.balsoft = {
    home.activation."mimeapps-remove" = {
      before = [ "checkLinkTargets" ];
      after = [ ];
      data = "rm -f /home/balsoft/.config/mimeapps.list";
    };

    xdg.mimeApps = {
      enable = true;
      defaultApplications = with config.defaultApplications;
        builtins.mapAttrs (name: value:
          if value ? desktop then [ "${value.desktop}.desktop" ] else value) {
            "inode/directory" = fm;
            "text/html" = browser;
            "image/*" = { desktop = "org.gnome.eog"; };
            "application/zip" = archive;
            "application/rar" = archive;
            "application/7z" = archive;
            "application/*tar" = archive;
            "x-scheme-handler/http" = browser;
            "x-scheme-handler/https" = browser;
            "x-scheme-handler/about" = browser;
            "x-scheme-handler/mailto" = mail;
            "x-scheme-handler/matrix" = matrix;
            "application/pdf" = { desktop = "org.kde.okular"; };
            "application/vnd.openxmlformats-officedocument.wordprocessingml.document" =
              text_processor;
            "application/msword" = text_processor;
            "application/vnd.oasis.opendocument.text" = text_processor;
            "text/csv" = spreadsheet;
            "application/vnd.oasis.opendocument.spreadsheet" = spreadsheet;
            "text/plain" =
              editor; # This actually makes Emacs an editor for everything... XDG is wierd
          };
    };
  };
}
