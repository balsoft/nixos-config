{ config, lib, pkgs, utils, ... }:
with lib;
with config.home-manager;
let cfg = themes;
in {
	options = {
		themes = {
			enable = mkEnableOption "theme management system";
			colors = mkOption {
				description = "Set of colors from which the themes for various applications will be generated";
				type = with types; submodule {
					options = {
						bg = mkOption {type = types.str;};
						fg = mkOption {type = types.str;};
						gray = mkOption {type = types.str;};
						alt = mkOption {type = types.str;};
						dark = mkOption {type = types.str;};
						blue = mkOption {type = types.str;};
						green = mkOption {type = types.str;};
						red = mkOption {type = types.str;};
						orange = mkOption {type = types.str;};
						yellow = mkOption {type = types.str;};
					};
				};
			};
		};
	};
}