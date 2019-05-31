{ config, lib, pkgs, ... }:
with lib;
let
  colorType = types.str;
  color = (name:
  (mkOption {
    description = "${name} color of palette";
    type = colorType;
  }));
  fromBase16 = 
  { base00, base01, base02, base03
  , base04, base05, base06, base07
  , base08, base09, base0A, base0B
  , base0C, base0D, base0E, base0F
  , ... }:
  {
    bg = base00;
    fg = base07;
    gray = base03;
    alt = base05;
    dark = base02;
    
    red = base08;
    orange = base09;
    yellow = base0A;
    green = base0B;
    cyan = base0C;
    blue = base0D;
    purple = base0E;
  };
in {
  options = {
    themes = {
      colors = mkOption {
        description =
          "Set of colors from which the themes for various applications will be generated";
        type = with types;
        submodule {
          options = {
            bg = color "background";
            fg = color "foreground";
            gray = color "gray";
            alt = color "alternative";
            dark = color "darker";
            blue = color "blue";
            green = color "green";
            red = color "red";
            orange = color "orange";
            yellow = color "yellow";
            cyan = color "cyan";
            purple = color "purple";
          };
        };
      };
    };
  };
  config = {
    themes.colors = {
      bg = "#272822";
      fg = "#F8F8F0";
      gray = "#75715E";
      alt = "#4f4d43";
      dark = "#222222";
      blue = "#5e81ac";
      green = "#A6E22E";
      red = "#F92672";
      orange = "#FD971F";
      yellow = "#E6DB74";
      purple = "#AE81FF";
      cyan = "#66D9EF";
    };
  };
}
