{ pkgs, config, lib, ... }:
with import ../support.nix { inherit lib config; }; {
  options.defaultApplications = lib.mkOption {
    type = lib.types.attrs;
    description = "Preferred applications";
  };
  config = rec {
    defaultApplications = {
      term = {
        cmd = "${pkgs.alacritty}/bin/alacritty";
        desktop = "alacritty";
      };
      editor = {
        cmd = "${config.home-manager.users.balsoft.programs.emacs.finalPackage}/bin/emacsclient -c $@";
        desktop = "emacsclient";
      };
      browser = {
        cmd = "${pkgs.firefox-wayland}/bin/firefox";
        desktop = "firefox";
      };
      fm = {
        cmd = "${pkgs.gnome3.nautilus}/bin/nautilus";
        desktop = "org.gnome.Nautilus";
      };
      monitor = {
        cmd = "${pkgs.gnome3.gnome-system-monitor}/bin/gnome-system-monitor";
        desktop = "gnome-system-monitor";
      };
      archive = {
        cmd = "${pkgs.gnome3.file-roller}/bin/file-roller";
        desktop = "org.gnome.FileRoller";
      };
      mail = {
        cmd = "${pkgs.gnome3.geary}/bin/geary";
        desktop = "org.gnome.Geary";
      };
      text_processor = {
        cmd = "${pkgs.abiword}/bin/abiword";
        desktop = "abiword";
      };
      spreadsheet = {
        cmd = "${pkgs.gnumeric}/bin/gnumeric";
        desktop = "gnumeric";
      };
    };

    environment.sessionVariables = {
      EDITOR = config.defaultApplications.editor.cmd;
      VISUAL = config.defaultApplications.editor.cmd;
    };

    home-manager.users.balsoft.xdg.mimeApps = {
      enable = true;
      defaultApplications =
        with config.defaultApplications;
        builtins.mapAttrs (name: value:
          if value ? desktop then [ "${value.desktop}.desktop" ] else value) {
            "text/html" = browser;
            "image/*" = { desktop = "org.gnome.eog"; };
            "application/zip" = archive;
            "application/rar" = archive;
            "application/7z" = archive;
            "application/*tar" = archive;
            "x-scheme-handler/http" = browser;
            "x-scheme-handler/https" = browser;
            "x-scheme-handler/about" = browser;
            "x-scheme-handler/unknown" = browser;
            "x-scheme-handler/mailto" = mail;
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
