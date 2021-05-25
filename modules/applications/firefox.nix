{ config, pkgs, lib, ... }:
let thm = config.themes.colors;
in {
  environment.sessionVariables = {
    MOZ_USE_XINPUT2 = "1";
    MOZ_DBUS_REMOTE = "1";
  };
  programs.browserpass.enable = true;
  home-manager.users.balsoft = lib.mkIf (config.deviceSpecific.goodMachine) {
    programs.browserpass = {
      enable = true;
      browsers = [ "firefox" ];
    };
    programs.firefox = {
      enable = true;
      package = pkgs.firefox-wayland;
      profiles.default = {
        id = 0;
        userChrome = ''
          #TabsToolbar {
            visibility: collapse;
          }
          toolbar#nav-bar, nav-bar-customization-target {
            background: ${thm.bg} !important;
          }
          @-moz-document url("about:newtab") {
            * { background-color: ${thm.bg}  !important; }
          }
        '';
        settings = {
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

          "extensions.autoDisableScopes" = 0;

          "browser.search.defaultenginename" = "Google";
          "browser.search.selectedEngine" = "Google";
          "browser.urlbar.placeholderName" = "Google";
          "browser.search.region" = "US";
          
          "browser.uidensity" = 1;
          "browser.search.openintab" = true;
          "xpinstall.signatures.required" = false;
          "extensions.update.enabled" = false;

          "font.name.monospace.x-western" = "IBM Plex Mono";
          "font.name.sans-serif.x-western" = "IBM Plex Sans";
          "font.name.serif.x-western" = "IBM Plex Serif";

          "browser.display.background_color" = thm.bg;
          "browser.display.foreground_color" = thm.fg;
          "browser.display.document_color_use" = 2;
          "browser.anchor_color" = thm.fg;
          "browser.visited_color" = thm.blue;
          "browser.display.use_document_fonts" = true;
          "pdfjs.disabled" = true;
          "media.videocontrols.picture-in-picture.enabled" = true;
        };
      };
      extensions = with pkgs.nur.rycee.firefox-addons; [
        torswitch
        adsum-notabs
        ublock-origin
        browserpass
      ];
    };
  };
}
