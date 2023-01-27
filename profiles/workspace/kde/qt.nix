{ pkgs, lib, config, ... }:
with pkgs.my-lib; {
  xdg.portal.enable = true;

  services.dbus.packages =
    [ pkgs.firefox pkgs.systemd pkgs.papirus-icon-theme ];
  services.udev.packages = [ pkgs.libmtp pkgs.media-player-info ];

  qt5.enable = false;

  environment.sessionVariables = {
    QT_XFT = "true";
    QT_SELECT = "5";
    KDE_SESSION_VERSION = "5";
    QT_SCALE_FACTOR = "1";
    QT_AUTO_SCREEN_SCALE_FACTOR = "0";
    QT_QPA_PLATFORMTHEME = "kde";
    KDEDIRS =
      "/run/current-system/sw:/run/current-system/sw/share/kservices5:/run/current-system/sw/share/kservicetypes5:/run/current-system/sw/share/kxmlgui5";
  };

  defaultApplications = {
    fm = {
      cmd = "${pkgs.dolphin}/bin/dolphin";
      desktop = "org.kde.dolphin";
    };
    archive = {
      cmd = "${pkgs.ark}/bin/ark";
      desktop = "org.kde.ark";
    };
  };

  home-manager.users.balsoft = let fonts = config.themes.fonts;
  in {
    home.packages = [ pkgs.ark pkgs.dolphin ];

    xdg.configFile."kdeglobals".text = with (thmDec config.themes.colors);
      lib.generators.toGitINI {
        "Colors:Button" = {
          BackgroundAlternate = base01;
          BackgroundNormal = base01;
          DecorationFocus = base02;
          DecorationHover = base02;
          ForegroundActive = base05;
          ForegroundInactive = base01;
          ForegroundLink = base0D;
          ForegroundNegative = base08;
          ForegroundNeutral = base09;
          ForegroundNormal = base05;
          ForegroundPositive = base0B;
          ForegroundVisited = base03;
        };
        "Colors:Complementary" = {
          BackgroundAlternate = base01;
          BackgroundNormal = base03;
          DecorationFocus = base02;
          DecorationHover = base02;
          ForegroundActive = base09;
          ForegroundInactive = base01;
          ForegroundLink = base0D;
          ForegroundNegative = base08;
          ForegroundNeutral = base0A;
          ForegroundNormal = base05;
          ForegroundPositive = base0B;
          ForegroundVisited = base02;
        };
        "Colors:Selection" = {
          BackgroundAlternate = base0D;
          BackgroundNormal = base0D;
          DecorationFocus = base0D;
          DecorationHover = base0D;
          ForegroundActive = base05;
          ForegroundInactive = base05;
          ForegroundLink = base0D;
          ForegroundNegative = base08;
          ForegroundNeutral = base09;
          ForegroundNormal = base05;
          ForegroundPositive = base0B;
          ForegroundVisited = base02;
        };
        "Colors:Tooltip" = {
          BackgroundAlternate = base01;
          BackgroundNormal = base00;
          DecorationFocus = base02;
          DecorationHover = base02;
          ForegroundActive = base02;
          ForegroundInactive = base01;
          ForegroundLink = base0D;
          ForegroundNegative = base08;
          ForegroundNeutral = base09;
          ForegroundNormal = base05;
          ForegroundPositive = base0B;
          ForegroundVisited = base03;
        };
        "Colors:View" = {
          BackgroundAlternate = base01;
          BackgroundNormal = base00;
          DecorationFocus = base02;
          DecorationHover = base02;
          ForegroundActive = base02;
          ForegroundInactive = base01;
          ForegroundLink = base0D;
          ForegroundNegative = base08;
          ForegroundNeutral = base09;
          ForegroundNormal = base05;
          ForegroundPositive = base0B;
          ForegroundVisited = base03;
        };
        "Colors:Window" = {
          BackgroundAlternate = base01;
          BackgroundNormal = base00;
          DecorationFocus = base02;
          DecorationHover = base02;
          ForegroundActive = base02;
          ForegroundInactive = base01;
          ForegroundLink = base0D;
          ForegroundNegative = base08;
          ForegroundNeutral = base09;
          ForegroundNormal = base05;
          ForegroundPositive = base0B;
          ForegroundVisited = base03;
        };
        General = {
          ColorScheme = "Generated";
          Name = "Generated";
          fixed = "${fonts.mono.family},${
              toString fonts.mono.size
            },-1,5,50,0,0,0,0,0";
          font = "${fonts.main.family},${
              toString fonts.main.size
            },-1,5,50,0,0,0,0,0";
          menuFont = "${fonts.main.family},${
              toString fonts.main.size
            },-1,5,50,0,0,0,0,0";
          shadeSortColumn = true;
          smallestReadableFont = "${fonts.main.family},${
              toString fonts.main.size
            },-1,5,57,0,0,0,0,0,Medium";
          toolBarFont = "${fonts.main.family},${
              toString fonts.main.size
            },-1,5,50,0,0,0,0,0";
          TerminalApplication = "alacritty";
        };
        KDE = {
          DoubleClickInterval = 400;
          ShowDeleteCommand = true;
          SingleClick = false;
          StartDragDist = 4;
          StartDragTime = 500;
          WheelScrollLines = 3;
          contrast = 4;
          widgetStyle = "Breeze";
        };
        Icons = { Theme = "Papirus-Dark"; };
      };
  };
}
