{ config, pkgs, lib, ... }:
let
  thm = pkgs.my-lib.thmHash;
  fonts = config.themes.fonts;
in {
  environment.sessionVariables = {
    MOZ_USE_XINPUT2 = "1";
    MOZ_DBUS_REMOTE = "1";
  };
  programs.browserpass.enable = true;

  persist.state.directories = [ "/home/balsoft/.mozilla/firefox/default" ];

  defaultApplications.browser = {
    cmd = "${pkgs.firefox-wayland}/bin/firefox";
    desktop = "firefox";
  };

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
            background: ${thm.base00} !important;
          }
          @-moz-document url("about:newtab") {
            * { background-color: ${thm.base00}  !important; }
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

          "font.name.monospace.x-western" = "${fonts.mono.family}";
          "font.name.sans-serif.x-western" = "${fonts.main.family}";
          "font.name.serif.x-western" = "${fonts.serif.family}";

          "browser.display.background_color" = thm.base00;
          "browser.display.foreground_color" = thm.base05;
          "browser.display.document_color_use" = 2;
          "browser.anchor_color" = thm.base0D;
          "browser.visited_color" = thm.base0C;
          "browser.display.use_document_fonts" = true;
          "pdfjs.disabled" = true;
          "media.videocontrols.picture-in-picture.enabled" = true;

          "widget.non-native-theme.enabled" = false;

          "browser.newtabpage.enabled" = false;
          "browser.startup.homepage" = "about:blank";
        };
      };
      extensions = with pkgs.nur.rycee.firefox-addons; [
        adsum-notabs
        ublock-origin
        browserpass
      ];
    };
  };
}
