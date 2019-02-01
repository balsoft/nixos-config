{pkgs, lib, config, ...}:
with import ../../../support.nix {inherit lib config;};
{
  services.flatpak.enable = true;
  services.flatpak.extraPortals = [pkgs.plasma5.xdg-desktop-portal-kde];
  services.dbus.packages = [pkgs.plasma5.xdg-desktop-portal-kde pkgs.flatpak pkgs.firefox];
  nixpkgs.config.firefox.enablePlasmaBrowserIntegration = true;
  home-manager.users.balsoft.home.packages = [ pkgs.qt5ct ];
  environment.sessionVariables =
  {
    DESKTOP_SESSION = "kde";
    QT_XFT = "true";
    QT_SELECT = "5";
    XDG_CURRENT_DESKTOP="KDE";
    KDE_SESSION_VERSION="5";
    QT_SCALE_FACTOR = "1";
    QT_AUTO_SCREEN_SCALE_FACTOR = "0";
    GTK_USE_PORTAL = "1";
    DE = "kde";
    QT_QPA_PLATFORMTHEME="qt5ct";
  };
  home-manager.users.balsoft.xdg.configFile."qt5ct/qt5ct.conf".text = genIni
  {
    Appearance =
    {
      custom_palette = false;
      icon_theme = "Papirus-Dark";
      standart_dialogs = "kde";
      style = "Breeze";
    };
    Fonts =
    {
      fixed = ''@Variant(\0\0\0@\0\0\0\x16\0R\0o\0\x62\0o\0t\0o\0 \0M\0o\0n\0o@&\0\0\0\0\0\0\xff\xff\xff\xff\x5\x1\0\x32\x10)'';
      general = ''@Variant(\0\0\0@\0\0\0\f\0R\0o\0\x62\0o\0t\0o@&\0\0\0\0\0\0\xff\xff\xff\xff\x5\x1\0\x32\x10)'';
    };
    Interface =
    {
      activate_item_on_single_click = "0";
      buttonbox_layout = "0";
      cursor_flash_time = "1000";
      dialog_buttons_have_icons = "1";
      double_click_interval = "400";
      menus_have_icons = true;
      toolbutton_style = "4";
      underline_shortcut = "1";
      wheel_scroll_lines = 1;
    };
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
