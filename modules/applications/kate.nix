{pkgs, config, lib, ...}:
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
  home-manager.users.balsoft = {
    xdg.configFile = {
      "katerc.home".text = genIni {
        General = {
          "Show Full Path in Title" = true;
          "Show Menu Bar" = false;
          "Show Status Bar" = true;
          "Show Tab Bar" = true;
        };
        "KTextEditor Renderer" = {
          "Animate Bracket Matching" = false;
          "Schema" = "Breeze Dark";
          "Show Indentation Lines" = true;
          "Show Whole Bracket Expression" = false;
          "Word Wrap Marker" = true;
        };
        UiSettings = {
          ColorScheme = "Nord";
        };
      };

      "kateschemarc".text = genIni {
        "Breeze Dark"."Color Background" = thmDec.bg;
      };
    };
    home.packages = [ pkgs.kate ];
    home.activation.konsole =
    {
      data = "$DRY_RUN_CMD cp -f ~/.config/katerc.home ~/.config/katerc";
      before = [];
      after = ["linkGeneration"];
    };
  };
}
