{ pkgs, lib, config, ... }:
with pkgs.my-lib;
let
  colorTheme = with (thmDec config.themes.colors); {
    "Colors:Button" = {
      BackgroundAlternate = base01;
      BackgroundNormal = base01;
      DecorationFocus = base0D;
      DecorationHover = base0D;
      ForegroundActive = base0D;
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
      DecorationFocus = base0D;
      DecorationHover = base0D;
      ForegroundActive = base0D;
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
      DecorationFocus = base0D;
      DecorationHover = base0D;
      ForegroundActive = base0D;
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
      DecorationFocus = base0D;
      DecorationHover = base0D;
      ForegroundActive = base0D;
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
      DecorationFocus = base0D;
      DecorationHover = base0D;
      ForegroundActive = base0D;
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
      shadeSortColumn = true;
    };
    KDE.contrast = 4;
    WM = {
      activeBackground = base00;
      activeBlend = base06;
      activeForeground = base05;
      inactiveBackground = base01;
      inactiveBlend = base02;
      inactiveForeground = base04;
    };
  };
  misc = with config.themes; {
    Icons.Theme = "breeze-dark";

    KDE = {
      DoubleClickInterval = 400;
      ShowDeleteCommand = true;
      SingleClick = false;
      StartDragDist = 4;
      StartDragTime = 500;
      WheelScrollLines = 3;
      widgetStyle = "Breeze";
    };
    General = {
      TerminalApplication = config.defaultApplications.term.cmd;
      fixed =
        "${fonts.mono.family},${toString fonts.mono.size},-1,5,50,0,0,0,0,0";
      font =
        "${fonts.main.family},${toString fonts.main.size},-1,5,50,0,0,0,0,0";
      menuFont =
        "${fonts.main.family},${toString fonts.main.size},-1,5,50,0,0,0,0,0";
      smallestReadableFont = "${fonts.main.family},${
          toString fonts.main.size
        },-1,5,57,0,0,0,0,0,Medium";
      toolBarFont =
        "${fonts.main.family},${toString fonts.main.size},-1,5,50,0,0,0,0,0";
    };
  };
  effects = with (thmDec config.themes.colors); {
    "ColorEffects:Disabled" = {
      Color = base02;
      ColorAmount = "0";
      ColorEffect = "0";
      ContrastAmount = "0.65";
      ContrastEffect = "1";
      IntensityAmount = "0.1";
      IntensityEffect = "2";
    };

    "ColorEffects:Inactive" = {
      ChangeSelectionColor = "true";
      Color = base03;
      ColorAmount = "0.025";
      ColorEffect = "2";
      ContrastAmount = "0.1";
      ContrastEffect = "2";
      Enable = "false";
      IntensityAmount = "0";
      IntensityEffect = "0";
    };
  };
  desktopThemeColors = pkgs.writeText "generated-plasma-theme-colors"
    (lib.generators.toGitINI
      (builtins.foldl' lib.recursiveUpdate { } [ colorTheme effects ]));
  desktopThemeRc = pkgs.writeText "generated-plasma-theme-rc"
    (lib.generators.toGitINI {
      Wallpaper = {
        defaultWallpaperTheme = "Next";
        defaultFileSuffix = ".png";
        defaultWidth = "1920";
        defaultHeight = "1080";
      };
      ContrastEffect = {
        enabled = "true";
        contrast = "0.17";
        intensity = "1.25";
        saturation = "9";
      };
      AdaptiveTransparency.enabled = "true";
    });
  desktopTheme = pkgs.linkFarm "generated-plasma-theme" [
    {
      name = "share/plasma/desktoptheme/generated/plasmarc";
      path = desktopThemeRc;
    }
    {
      name = "share/plasma/desktoptheme/generated/colors";
      path = desktopThemeColors;
    }
  ];
in {
  environment.systemPackages = [ desktopTheme ];

  xdg.portal.enable = true;

  services.dbus.packages = [ pkgs.systemd pkgs.breeze-icons ];
  services.udev.packages = [ pkgs.libmtp pkgs.media-player-info ];

  qt.enable = false;

  environment.sessionVariables = {
    QT_XFT = "true";
    QT_SELECT = "5";
    KDE_SESSION_VERSION = "5";
    QT_SCALE_FACTOR = "1";
    QT_AUTO_SCREEN_SCALE_FACTOR = "0";
    QT_QPA_PLATFORMTHEME = "kde";
    QT_QUICK_CONTROLS_STYLE = "org.kde.breeze";
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

  home-manager.users.balsoft = {
    home.packages = [ pkgs.ark pkgs.dolphin ];

    xdg.configFile."kdeglobals".text = lib.generators.toGitINI
      (builtins.foldl' lib.recursiveUpdate { } [ colorTheme effects misc ]);
  };
}
