{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  thm = pkgs.my-lib.thmHash config.themes.colors;
  fonts = config.themes.fonts;
in
{
  services.dbus.packages = [ pkgs.librewolf ];

  environment.sessionVariables = {
    MOZ_USE_XINPUT2 = "1";
    MOZ_DBUS_REMOTE = "1";
  };
  programs.browserpass.enable = true;

  persist.state.directories = [ "/home/balsoft/.librewolf/default" ];

  defaultApplications.browser = {
    cmd = "${pkgs.librewolf}/bin/librewolf";
    desktop = "librewolf";
  };

  home-manager.users.balsoft = {
    programs.browserpass = {
      enable = true;
      browsers = [ "librewolf" ];
    };
    wayland.windowManager.sway.config = {
      window.commands = [
        {
          criteria = {
            title = "Firefox — Sharing Indicator";
          };
          command = "floating enable";
        }
        {
          criteria = {
            title = "Firefox — Sharing Indicator";
          };
          command = "no_focus";
        }
        {
          criteria = {
            title = "Firefox — Sharing Indicator";
          };
          command = "resize set 0 0";
        }
        {
          criteria = {
            title = "Firefox — Sharing Indicator";
          };
          command = "move absolute position 10 10";
        }
      ];
    };

    # home.file.".mozilla/native-messaging-hosts/tridactyl.json".text = let
    #   tridactyl = with pkgs.nimPackages;
    #     buildNimPackage {
    #       pname = "tridactyl_native";
    #       version = "dev";
    #       nimBinOnly = true;
    #       src = inputs.tridactyl-native-messenger;
    #       buildInputs = [ tempfile regex unicodedb ];
    #     };
    # in builtins.toJSON {
    #   name = "tridactyl";
    #   description = "Tridactyl native command handler";
    #   path = "${tridactyl}/bin/native_main";
    #   type = "stdio";

    #   allowed_extensions = [
    #     "tridactyl.vim@cmcaine.co.uk"
    #     "tridactyl.vim.betas@cmcaine.co.uk"
    #     "tridactyl.vim.betas.nonewtab@cmcaine.co.uk"
    #   ];
    # };

    programs.librewolf = {
      enable = true;
      package = pkgs.librewolf;
      profiles.default = {
        extensions.packages = with pkgs.nur.rycee.firefox-addons; [
          adsum-notabs
          ublock-origin
          sponsorblock
          browserpass
          darkreader
        ];
        id = 0;
        userChrome = ''
          #TabsToolbar {
            visibility: collapse;
          }
          // toolbar#nav-bar, nav-bar-customization-target {
          //   background: ${thm.base00} !important;
          // }
          // @-moz-document url("about:newtab") {
          //   * { background-color: ${thm.base00}  !important; }
          // }
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

          "browser.anchor_color" = thm.base0D;
          "browser.visited_color" = thm.base0C;
          "browser.display.use_document_fonts" = true;
          "pdfjs.disabled" = true;
          "media.videocontrols.picture-in-picture.enabled" = true;

          "widget.non-native-theme.enabled" = false;

          "browser.newtabpage.enabled" = false;
          "browser.startup.homepage" = "about:blank";

          "browser.newtabpage.activity-stream.feeds.telemetry" = false;
          "browser.newtabpage.activity-stream.telemetry" = false;
          "browser.ping-centre.telemetry" = false;
          "toolkit.telemetry.archive.enabled" = false;
          "toolkit.telemetry.bhrPing.enabled" = false;
          "toolkit.telemetry.enabled" = false;
          "toolkit.telemetry.firstShutdownPing.enabled" = false;
          "toolkit.telemetry.hybridContent.enabled" = false;
          "toolkit.telemetry.newProfilePing.enabled" = false;
          "toolkit.telemetry.reportingpolicy.firstRun" = false;
          "toolkit.telemetry.shutdownPingSender.enabled" = false;
          "toolkit.telemetry.unified" = false;
          "toolkit.telemetry.updatePing.enabled" = false;

          "experiments.activeExperiment" = false;
          "experiments.enabled" = false;
          "experiments.supported" = false;
          "network.allow-experiments" = false;

          "privacy.clearOnShutdown_v2.cookiesAndStorage" = false;
        };
        search = {
          default = "ddg";
          engines = {
            # hide bullshit
            amazon.metaData.hidden = true;
            amazondotcom-us.metaData.hidden = true;
            ebay.metaData.hidden = true;
            policy-MetaGer.metaData.hidden = true;
            policy-StartPage.metaData.hidden = true;
            policy-Mojeek.metaData.hidden = true;
            mojeek.metaData.hidden = true;
            bing.metaData.hidden = true;


            # Assign aliases to internal engines
            ddg.metaData.alias = "@d";
            "policy-DuckDuckGo Lite".metaData.alias = "@l";
            wikipedia.metaData.alias = "@w";
            # don't hide this bullshit because sadly it's useful
            google.metaData.alias = "@g";

            # Add custom engines
            wikipedia-ru = {
              name = "Wikipedia (ru)";
              urls = [
                {
                  template = "https://ru.wikipedia.org/w/index.php?search={searchTerms}";
                }
              ];
              iconMapObj."16" = "https://ru.wikipedia.org/favicon.ico";

              definedAliases = [ "@wr" ];
            };

            nixos-wiki = {
              name = "NixOS Wiki";
              urls = [ { template = "https://wiki.nixos.org/w/index.php?search={searchTerms}"; } ];
              definedAliases = [ "@nw" ];
              iconMapObj."16" = "https://wiki.nixos.org/favicon.ico";
            };

            nix-packages = {
              name = "Nixpkgs";
              urls = [
                {
                  template = "https://search.nixos.org/packages?query={searchTerms}";
                }
              ];
              iconMapObj."16" = "https://search.nixos.org/favicon.png";
              definedAliases = [ "@np" ];
            };

            nixos-options = {
              name = "NixOS";
              urls = [
                {
                  template = "https://search.nixos.org/options?query={searchTerms}";
                }
              ];
              iconMapObj."16" = "https://search.nixos.org/favicon.png";
              definedAliases = [ "@no" ];
            };

            nix-flakes = {
              name = "Nix Flakes";
              urls = [
                {
                  template = "https://search.nixos.org/flakes?query={searchTerms}";
                }
              ];
              iconMapObj."16" = "https://search.nixos.org/favicon.png";
              definedAliases = [ "@nf" ];
            };

            github = {
              name = "GitHub";
              urls = [
                {
                  template = "https://github.com/search?type=code&q={searchTerms}";
                }
              ];
              iconMapObj."16" = "https://github.com/favicon.ico";
              definedAliases = [ "@gh" ];
            };

            chatgpt = {
              name = "ChatGPT";
              urls = [
                { template = "https://chatgpt.com/?q={searchTerms}"; }
              ];
              iconMapObj."16" = "https://chatgpt.com/favicon.ico";
              definedAliases = [ "@c" ];
            };
          };
          force = true;
        };
      };
    };
  };
}
