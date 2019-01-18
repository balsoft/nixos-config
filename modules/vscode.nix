{ config, lib, pkgs, utils, ... }:
with lib;
let cfg = config.programs.vscode; in
{
	options = {
		programs.vscode = {
			enable = mkEnableOption "Visual Studio Code";
			extensions = mkOption {
				type = types.listOf types.str;
				description = "List of vscode extensions to install";
				default = [];
			};
			userSettings = mkOption {
				type = types.nullOr types.attrs;
				description = "User settings of vscode";
			};
		};
	};
	config = lib.mkIf cfg.enable {
		xdg.configFile."Code/Usersettings.json" = lib.mkIf ! isNull cfg.userSettings{
			text = builtins.toJSON cfg.userSettings;
		};
		home.activation.vscode = {
			after = [ "linkGeneration" ];
			before = [];
			data = builtins.concatStringsSep " || echo 'Error'\n" (map (ext: "$DRY_RUN_CMD code --install-extension ${ext}") cfg.extensions );
		};
	};
}