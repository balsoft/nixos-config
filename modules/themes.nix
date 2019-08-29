{ config, lib, pkgs, ... }:
with lib;
let
  colorType = types.str;
  color = (name:
  (mkOption {
    description = "${name} color of palette";
    type = colorType;
  }));
  fromBase16 = { base00, base01, base02, base03, base04, base05, base06, base07
  , base08, base09, base0A, base0B, base0C, base0D, base0E, base0F, ... }:
  builtins.mapAttrs (_: v: "#" + v) {
    bg = base00;
    fg = base07;

    gray = base03;
    alt = base02;
    dark = base01;

    red = base08;
    orange = base09;
    yellow = base0A;
    green = base0B;
    cyan = base0C;
    blue = base0D;
    purple = base0E;
  };
  
  fromYAML = yaml:
  builtins.fromJSON (
    builtins.readFile (
      pkgs.stdenv.mkDerivation {
        name = "fromYAML";
        phases = ["buildPhase"];
        buildPhase = ''echo '${yaml}' | ${pkgs.yaml2json}/bin/yaml2json > $out'';
      }
    )
  );
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
    # themes.colors = fromBase16 (fromYAML (builtins.readFile
    # ../imports/github/dawikur/base16-gruvbox-scheme/gruvbox-dark-hard.yaml));
    themes.colors = {
      # bg = "#114c00";
      bg = "#000000";
      fg = "#ffffff";

      gray = "#9bb953";
      alt = "#27a6a2";
      dark = "#169300";

      red = "#d12d17";
      orange = "#cf7a02";
      yellow = "#d0c900";
      green = "#ccd0c6";
      cyan = "#09d0a5";
      blue = "#a5d048";
      purple = "#d083be";
    };
  };
}
