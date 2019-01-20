{ config, lib, pkgs, ... }:
with lib;
let
  colorType = types.str;
  color = (name: (mkOption {description = "${name} color of palette"; type = colorType;}));
in
{
	options =
  {
		themes =
    {
			colors = mkOption
      {
				description = "Set of colors from which the themes for various applications will be generated";
				type = with types; submodule
        {
					options =
          {
						bg = color "background";
						fg = color "foreground";
						gray = color "gray";
						alt = color "alternative";
						dark = color "darker";
						blue = color "blue" ;
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
  config =
  {
    themes.colors =
    {
      bg = "#2e3440";
      fg = "#d8dee9";
      gray = "#7f8c8d";
      alt = "#4c566a";
      dark = "#3b4252";
      blue = "#5e81ac";
      green = "#a3be8c";
      red = "#bf616a";
      orange = "#d08770";
      yellow = "#ebcb8b";
      purple = "#b48ead";
      cyan = "#88c0d0";
    };
  };
}
