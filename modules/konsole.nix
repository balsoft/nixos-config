{ config, pkgs, libs, ... }:
with lib;
{
  options = {
    konsole = {
      enable = mkEnableOption "konsole, the KDE terminal application";
      config = mkOption {
        description = "Configuration of Konsole. Will not directly link, because Konsole is not happy with not being able to write to its config."
        type = types.attrSet;
        default = {
          "Desktop Entry".DefaultProfile = "Default.profile";
          KonsoleWindow.ShowMenuBarByDefault = false;
        };
      };
      profile = mkOption {
        description = "Default profile of Konsole";
        type = types.attrSet;
        default = {
          Appearance.ColorScheme = mkIf config.themes.enable "generated";
          "Cursor Options".CursorShape = 1;
          General = {
            Command = environment.sessionVariables.SHELL;
            Name = "Default";
            Parent = "FALLBACK/";
          };
          Scrolling.HistoryMode = 2;
          "Terminal Features".BlinkingCursorEnabled = true;
        };
      };
      colorScheme = mkOption {
        description = "Default color scheme of Konsole";
        type = types.attrSet;
        default = {
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
    };
  };
}
