{pkgs, lib, config, ...}:
with (import ../../support.nix {inherit lib;});
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
  home-manager.users.balsoft.xdg.dataFile = {
    "konsole/Default.profile".text = genIni {
      Appearance.ColorScheme = "generated";
      "Cursor Options".CursorShape = 1;
      General = {
        Command = "zsh";
        Name = "Default";
        Parent = "FALLBACK/";
      };
      Scrolling.HistoryMode = 2;
      "Terminal Features".BlinkingCursorEnabled = true;
    };
    "konsole/generated.colorscheme".text = genIni {
      General = {
        Description = "generated";
        Opacity = 1;
        Wallpaper = "";
      };
      Background.Color = thmDec.bg;
      BackgroundIntense.Color = thmDec.bg;
      Foreground.Color = thmDec.fg;
      Color0.Color = thmDec.dark;
      Color0Intense.Color = thmDec.alt;
      Color1.Color = thmDec.red;
      Color2.Color = thmDec.green;
      Color3.Color = thmDec.yellow;
      Color4.Color = thmDec.blue;
      Color5.Color = thmDec.purple;
      Color6.Color = thmDec.cyan;
      Color7.Color = thmDec.fg;
    };
    
  };
  home-manager.users.balsoft.xdg.configFile."konsolerc.home".text = genIni {
    "Desktop Entry".DefaultProfile = "Default.profile";
     KonsoleWindow.ShowMenuBarByDefault = false;
  };
  home-manager.users.balsoft.home.activation.konsole = {
    data = "$DRY_RUN_CMD cp -f ~/.config/konsolerc.home ~/.config/konsolerc";
    before = [];
    after = [ "linkGeneration" ];
  };
}
