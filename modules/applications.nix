{ pkgs, config, lib, ... }:
with import ../support.nix { inherit lib config; }; {
  options.defaultApplications = lib.mkOption {
    type = lib.types.attrs;
    description = "Preferred applications";
  };
  config = rec {
    defaultApplications = {
      term = {
        cmd = "${pkgs.konsole}/bin/konsole";
        desktop = "konsole";
      };
      editor = {
        cmd = toString (pkgs.writeTextFile {
          name = "emacsclient";
          text = ''
            #!${pkgs.bash}/bin/bash
            ${config.home-manager.users.balsoft.programs.emacs.finalPackage}/bin/emacsclient -s /tmp/emacs1000/server -c $@'';
          executable = true;
        });
        desktop = "emacsclient";
      };
      browser = {
        cmd = "${pkgs.firefox-wayland}/bin/firefox";
        desktop = "firefox";
      };
      fm = {
        cmd = "${pkgs.dolphin}/bin/dolphin";
        desktop = "dolphin";
      };
      monitor = {
        cmd = "${pkgs.ksysguard}/bin/ksysguard";
        desktop = "ksysguard";
      };
      torrent = {
        cmd = "${pkgs.ktorrent}/bin/ktorrent";
        desktop = "ktorrent";
      };
      archive = {
        cmd = "${pkgs.ark}/bin/ark";
        desktop = "org.kde.ark";
      };
      mail = {
        cmd = "${pkgs.sylpheed}/bin/sylpheed";
        desktop = "sylpheed";
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
    home-manager.users.balsoft.xdg.mimeApps = {
      enable = true;
      defaultApplications =
        with config.defaultApplications;
        builtins.mapAttrs (name: value:
          if value ? desktop then [ "${value.desktop}.desktop" ] else value) {
            "text/html" = browser;
            "image/*" = { desktop = "org.kde.gwenview"; };
            "application/x-bittorrent" = torrent;
            "application/zip" = archive;
            "application/rar" = archive;
            "application/7z" = archive;
            "application/*tar" = archive;
            "application/x-kdenlive" = archive;
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
