{ pkgs, lib, config, ... }:
with import ../../../support.nix { inherit lib config; }; {
  xdg.portal.enable = true;
  services.dbus.packages = [
    pkgs.firefox
    pkgs.systemd
    pkgs.papirus-icon-theme
  ];
  services.udev.packages = [
    pkgs.libmtp
    pkgs.media-player-info
  ];

  environment.sessionVariables = {
    DESKTOP_SESSION = "kde";
    QT_XFT = "true";
    QT_SELECT = "5";
    XDG_CURRENT_DESKTOP = "KDE";
    KDE_SESSION_VERSION = "5";
    QT_SCALE_FACTOR = "1";
    QT_AUTO_SCREEN_SCALE_FACTOR = "0";
    DE = "kde";
    QT_QPA_PLATFORMTHEME = "kde";
    KDEDIRS = "/run/current-system/sw:/run/current-system/sw/share/kservices5:/run/current-system/sw/share/kservicetypes5:/run/current-system/sw/share/kxmlgui5";
  };
  home-manager.users.balsoft.xdg.configFile."kdeglobals".text = genIni {
    "Colors:Button" = {
      BackgroundAlternate = thmDec.dark;
      BackgroundNormal = thmDec.bg;
      DecorationFocus = thmDec.alt;
      DecorationHover = thmDec.alt;
      ForegroundActive = thmDec.alt;
      ForegroundInactive = thmDec.dark;
      ForegroundLink = thmDec.blue;
      ForegroundNegative = thmDec.red;
      ForegroundNeutral = thmDec.orange;
      ForegroundNormal = thmDec.fg;
      ForegroundPositive = thmDec.green;
      ForegroundVisited = thmDec.gray;
    };
    "Colors:Complementary" = {
      BackgroundAlternate = thmDec.dark;
      BackgroundNormal = thmDec.bg;
      DecorationFocus = thmDec.alt;
      DecorationHover = thmDec.alt;
      ForegroundActive = thmDec.orange;
      ForegroundInactive = thmDec.dark;
      ForegroundLink = thmDec.blue;
      ForegroundNegative = thmDec.red;
      ForegroundNeutral = thmDec.yellow;
      ForegroundNormal = thmDec.fg;
      ForegroundPositive = thmDec.green;
      ForegroundVisited = thmDec.alt;
    };
    "Colors:Selection" = {
      BackgroundAlternate = thmDec.alt;
      BackgroundNormal = thmDec.alt;
      DecorationFocus = thmDec.alt;
      DecorationHover = thmDec.alt;
      ForegroundActive = thmDec.fg;
      ForegroundInactive = thmDec.fg;
      ForegroundLink = thmDec.blue;
      ForegroundNegative = thmDec.red;
      ForegroundNeutral = thmDec.orange;
      ForegroundNormal = thmDec.fg;
      ForegroundPositive = thmDec.green;
      ForegroundVisited = thmDec.alt;
    };
    "Colors:Tooltip" = {
      BackgroundAlternate = thmDec.dark;
      BackgroundNormal = thmDec.bg;
      DecorationFocus = thmDec.alt;
      DecorationHover = thmDec.alt;
      ForegroundActive = thmDec.alt;
      ForegroundInactive = thmDec.dark;
      ForegroundLink = thmDec.blue;
      ForegroundNegative = thmDec.red;
      ForegroundNeutral = thmDec.orange;
      ForegroundNormal = thmDec.fg;
      ForegroundPositive = thmDec.green;
      ForegroundVisited = thmDec.gray;
    };
    "Colors:View" = {
      BackgroundAlternate = thmDec.dark;
      BackgroundNormal = thmDec.bg;
      DecorationFocus = thmDec.alt;
      DecorationHover = thmDec.alt;
      ForegroundActive = thmDec.alt;
      ForegroundInactive = thmDec.dark;
      ForegroundLink = thmDec.blue;
      ForegroundNegative = thmDec.red;
      ForegroundNeutral = thmDec.orange;
      ForegroundNormal = thmDec.fg;
      ForegroundPositive = thmDec.green;
      ForegroundVisited = thmDec.gray;
    };
    "Colors:Window" = {
      BackgroundAlternate = thmDec.dark;
      BackgroundNormal = thmDec.bg;
      DecorationFocus = thmDec.alt;
      DecorationHover = thmDec.alt;
      ForegroundActive = thmDec.alt;
      ForegroundInactive = thmDec.dark;
      ForegroundLink = thmDec.blue;
      ForegroundNegative = thmDec.red;
      ForegroundNeutral = thmDec.orange;
      ForegroundNormal = thmDec.fg;
      ForegroundPositive = thmDec.green;
      ForegroundVisited = thmDec.gray;
    };
    General = {
      ColorScheme = "Generated";
      Name = "Generated";
      fixed = "IBM Plex Mono,11,-1,5,50,0,0,0,0,0";
      font = "IBM Plex,11,-1,5,50,0,0,0,0,0";
      menuFont = "IBM Plex,11,-1,5,50,0,0,0,0,0";
      shadeSortColumn = true;
      smallestReadableFont = "IBM Plex,8,-1,5,57,0,0,0,0,0,Medium";
      toolBarFont = "IBM Plex,11,-1,5,50,0,0,0,0,0";
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
}
