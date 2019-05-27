{ config, lib, pkgs, ... }:
with lib;
let
  colorType = types.str;
  color = (name:
  (mkOption {
    description = "${name} color of palette";
    type = colorType;
  }));
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
