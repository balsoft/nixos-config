{ config, pkgs, lib, ... }:
with import ../../support.nix { inherit lib config; };
let thm = config.themes.colors;
in {
  environment.sessionVariables.MOZ_USE_XINPUT2 = "1";
  home-manager.users.balsoft = lib.mkIf (config.deviceSpecific.goodMachine) {
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
          "browser.search.selectedEnging" = "Google";
          "browser.urlbar.placeholderName" = "Google";
          "browser.search.region" = "US";
          
          "browser.uidensity" = 1;
          "browser.search.openintab" = true;
          "accessibility.browsewithcaret" = true;
          "extensions.pocket.enabled" = false;
          "datareporting.healthreport.uploadEnabled" = false;
          "browser.ping-centre.telemetry" = false;
          "browser.safebrowsing.blockedURIs.enabled" = false;
          "browser.safebrowsing.downloads.enabled" = false;
          "browser.safebrowsing.malware.enabled" = false;
          "browser.safebrowsing.phishing.enabled" = false;
          "network.tcp.tcp_fastopen_enable" = true;
          "xpinstall.signatures.required" = false;
          "extensions.update.enabled" = false;

          "network.trr.mode" = 5;
          "network.trr.ui" = "https://mozilla.cloudflare-dns.com/dns-query";
          "network.trr.bootstrapAddress" = "1.1.1.1";

          "font.name.monospace.x-western" = "Roboto Mono";
          "font.name.sans-serif.x-western" = "Roboto";
          "font.name.serif.x-western" = "Roboto Slab";

          "browser.display.background_color" = thm.bg;
          "browser.display.foreground_color" = thm.fg;
          "browser.display.document_color_use" = 2;
          "browser.anchor_color" = thm.fg;
          "browser.visited_color" = thm.blue;
          "browser.display.use_document_fonts" = true;
          "pdfjs.disabled" = true;
        };
      };
      extensions = with pkgs.nur.rycee.firefox-addons; [
        torswitch
        close-other-windows
        adsum-notabs
        ublock-origin
        plasma-integration
      ];
    };
  };
}
