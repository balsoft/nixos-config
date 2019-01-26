{pkgs, lib, config, ...}:
with import ../../support.nix {inherit lib;};
let 
  thm = config.themes.colors;
  splitHex = hexStr: map (x: builtins.elemAt x 0) (builtins.filter (a: a != "" && a != []) (builtins.split "(.{2})" (builtins.substring 1 6 hexStr)));
  hex2decDigits = {
    "0" = 0;  
    "1" = 1;
    "2" = 2;
    "3" = 3;
    "4" = 4;
    "5" = 5;
    "6" = 6;
    "7" = 7;
    "8" = 8;
    "9" = 9;
    "a" = 10;
    "b" = 11;
    "c" = 12;
    "d" = 13;
    "e" = 14;
    "f" = 15;
  };

  doubleDigitHexToDec = hex: 16 * hex2decDigits."${builtins.substring 0 1 hex}" + hex2decDigits."${builtins.substring 1 2 hex}";
  thmDec = builtins.mapAttrs (name: color: colorHex2Dec color) thm;
  colorHex2Dec = color: builtins.concatStringsSep "," (map (x: toString (doubleDigitHexToDec x)) (splitHex color));
in
{
  services.flatpak.enable = true;
  services.flatpak.extraPortals = [pkgs.plasma5.xdg-desktop-portal-kde];
  services.dbus.packages = [pkgs.plasma5.xdg-desktop-portal-kde pkgs.flatpak pkgs.firefox];
  environment.sessionVariables =
  {
    DESKTOP_SESSION = "kde";
    QT_XFT = "true";
    QT_SELECT = "5";
    XDG_CURRENT_DESKTOP="KDE";
    KDE_SESSION_VERSION="5";
    QT_QPA_PLATFORMTHEME = "kde";
    QT_SCALE_FACTOR = "1";
    QT_AUTO_SCREEN_SCALE_FACTOR = "0";
    GTK_USE_PORTAL = "1";
    DE = "kde";
  };
  home-manager.users.balsoft.xdg.configFile."kdeglobals".text = genIni {
    "Colors:Button" = {
       BackgroundAlternate = thmDec.dark;
       BackgroundNormal = thmDec.bg;
       DecorationFocus = thmDec.blue;
       DecorationHover = thmDec.blue;
       ForegroundActive = thmDec.blue;
       ForegroundInactive = thmDec.alt;
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
      DecorationFocus = thmDec.blue;
      DecorationHover = thmDec.blue;
      ForegroundActive = thmDec.orange;
      ForegroundInactive = thmDec.alt;
      ForegroundLink = thmDec.blue;
      ForegroundNegative = thmDec.red;
      ForegroundNeutral = thmDec.yellow;
      ForegroundNormal = thmDec.fg;
      ForegroundPositive = thmDec.green;
      ForegroundVisited = thmDec.blue;
    };
    "Colors:Selection" = {
      BackgroundAlternate = thmDec.blue;
      BackgroundNormal = thmDec.blue;
      DecorationFocus = thmDec.blue;
      DecorationHover = thmDec.blue;
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
      DecorationFocus = thmDec.blue;
      DecorationHover = thmDec.blue;
      ForegroundActive = thmDec.blue;
      ForegroundInactive = thmDec.alt;
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
      DecorationFocus = thmDec.blue;
      DecorationHover = thmDec.blue;
      ForegroundActive = thmDec.blue;
      ForegroundInactive = thmDec.alt;
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
      DecorationFocus = thmDec.blue;
      DecorationHover = thmDec.blue;
      ForegroundActive = thmDec.blue;
      ForegroundInactive = thmDec.alt;
      ForegroundLink = thmDec.blue;
      ForegroundNegative = thmDec.red;
      ForegroundNeutral = thmDec.orange;
      ForegroundNormal = thmDec.fg;
      ForegroundPositive = thmDec.green;
      ForegroundVisited = thmDec.gray;
    };
    General = {
      ColorScheme="Generated";
      Name="Generated";
      fixed = "Roboto Mono,11,-1,5,50,0,0,0,0,0";
      font = "Roboto,11,-1,5,50,0,0,0,0,0";
      menuFont = "Roboto,11,-1,5,50,0,0,0,0,0";
      shadeSortColumn = true;
      smallestReadableFont = "Roboto,8,-1,5,57,0,0,0,0,0,Medium";
      toolBarFont = "Roboto,11,-1,5,50,0,0,0,0,0";
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
    Icons = {
        Theme="Papirus-Dark";
    };
  };
  home-manager.users.balsoft.home.activation."user-places.xbel" =
  {
    data = "$DRY_RUN_CMD cp ${./user-places.xbel} ~/.local/share/user-places.xbel";
    before = [];
    after = ["linkGeneration"];
  };
}
