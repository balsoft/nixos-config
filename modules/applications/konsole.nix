{pkgs, lib, config, ...}:
with (import ../../support.nix {inherit lib config;});
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
