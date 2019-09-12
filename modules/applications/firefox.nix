{ config, pkgs, lib, ... }:
with import ../../support.nix { inherit lib config; };
let thm = config.themes.colors;
in {
  environment.sessionVariables.MOZ_USE_XINPUT2 = "1";

  home-manager.users.balsoft = {
    home.file.".mozilla/firefox/profiles.ini".text = genIni {
      General.StartWithLastProfile = 1;
      Profile0 = {
        Name = "default";
        IsRelative = 1;
        Path = "profile.default";
        Default = 1;
      };
    };
    home.file.".mozilla/firefox/profile.default/user.js".text = ''
      pref("extensions.autoDisableScopes", 0);
      pref("browser.search.defaultenginename", "Google");
      pref("browser.search.selectedEngine", "Google");
      pref("browser.uidensity", 1);
      pref("browser.search.openintab", true);
      pref("accessibility.browsewithcaret", true);
      pref("extensions.pocket.enabled", false);  
      pref("datareporting.healthreport.uploadEnabled", false);
      pref("browser.ping-centre.telemetry", false);
      pref("browser.safebrowsing.blockedURIs.enabled", false);
      pref("browser.safebrowsing.downloads.enabled", false);
      pref("browser.safebrowsing.malware.enabled", false);
      pref("browser.safebrowsing.phishing.enabled", false);
      pref("network.tcp.tcp_fastopen_enable", true);
      pref("xpinstall.signatures.required", false);
      pref("extensions.update.enabled", false);

      pref("network.trr.mode", 3);
      pref("network.trr.uri", "https://mozilla.cloudflare-dns.com/dns-query");
      pref("network.trr.bootstrapAddress", "1.1.1.1")

      pref("font.name.monospace.x-western", "Roboto Mono");
      pref("font.name.sans-serif.x-western", "Roboto");
      pref("font.name.serif.x-western", "Roboto Slab");

      pref("browser.tabs.remote.autostart", false);
      pref("browser.display.background_color", "${thm.bg}");
      pref("browser.display.document_color_use", 2);
      pref("browser.display.foreground_color", "${thm.fg}");
      pref("browser.anchor_color", "${thm.fg}");
      pref("browser.visited_color", "${thm.blue}");
      pref("browser.display.use_document_fonts", 0);
      pref("browser.display.use_system_colors", true);
      pref("pdfjs.disabled", true);
    '';
    home.file.".mozilla/firefox/profile.default/chrome/userChrome.css".text = ''
      #TabsToolbar {
      visibility: collapse;
      }
      toolbar#nav-bar, nav-bar-customization-target {
      background: ${thm.bg} !important;
      }
      @-moz-document url("about:newtab") {
      * { background-color: ${thm.bg}  !important;}
      }
    '';
    home.file.".mozilla/firefox/profile.default/extensions/uBlock0@raymondhill.net.xpi".source =
    pkgs.fetchurl {
      url =
        https://addons.mozilla.org/firefox/downloads/file/1166954/ublock_origin-1.17.4-an+fx.xpi;
      sha256 =
        "54c9a1380900eb1eba85df3a82393cef321e9c845fda227690d9377ef30e913e";
    };
    home.file.".mozilla/firefox/profile.default/extensions/{c9f848fb-3fb6-4390-9fc1-e4dd4d1c5122}.xpi".source =
    pkgs.fetchurl {
      url =
        https://addons.mozilla.org/firefox/downloads/file/883289/no_tabs-1.1-an+fx-linux.xpi;
      sha256 =
        "48e846a60b217c13ee693ac8bfe23a8bdef2ec073f5f713cce0e08814f280354";
    };
    home.file.".mozilla/firefox/profile.default/extensions/keepassxc-browser@keepassxc.org.xpi".source =
    pkgs.fetchurl {
      url =
        https://addons.mozilla.org/firefox/downloads/file/1205950/keepassxc_browser-1.3.2-fx.xpi;
      sha256 =
        "8a9c13f36b6ea8c5287ea6f99a8a9dc8c28b615c529e44d630221c03aee26790";
    };
    home.file.".mozilla/firefox/profile.default/extensions/{fab4ea0f-e0d3-4bb4-9515-aea14d709f69}.xpi".source =
    pkgs.fetchurl {
      url =
        https://addons.mozilla.org/firefox/downloads/file/589832/close_other_windows-0.1-an+fx-linux.xpi;
      sha256 =
        "6c189fb4d396f835bf8f0f09c9f1e9ae5dc7cde471b776d8c7d12592a373d3d3";
    };
    home.file.".mozilla/firefox/profile.default/extensions/{34fab4dc-77cc-4631-be8b-7a85a1e9fc09}.xpi".source =
    pkgs.fetchurl {
      url =
        https://addons.mozilla.org/firefox/downloads/file/1020346/torswitch-1.0-an+fx.xpi;
      sha256 =
        "3c50bd5c8890628a7260a742099293b6e752e7826e0643e3f515105ec3d9b85e";
    };
  };
}
