{ config, pkgs, lib, ... }: {
  environment.systemPackages = [ pkgs.plasma5Packages.qmlkonsole ];
  defaultApplications.term = {
    cmd = "${pkgs.plasma5Packages.qmlkonsole}/bin/qmlkonsole";
    desktop = "org.kde.qmlkonsole";
  };
  environment.variables.COLORSCHEMES_DIRS = pkgs.writeTextFile {
    name = "qmlkonsole-generated-theme";
    destination = "/Generated.colorscheme";
    text = with pkgs.my-lib;
      with (thmDec config.themes.colors);
      let dim = mulDec 0.66;
      in lib.generators.toGitINI {
        General.Description = "Generated Color Scheme";
        Background = {
          Bold = false;
          Color = base00;
        };
        BackgroundIntense = {
          Bold = false;
          Color = base03;
        };
        Color0 = {
          Bold = false;
          Color = base00;
        };
        Color1 = {
          Bold = false;
          Color = base08;
        };
        Color2 = {
          Bold = false;
          Color = base0B;
        };
        Color3 = {
          Bold = false;
          Color = base0A;
        };
        Color4 = {
          Bold = false;
          Color = base0D;
        };
        Color5 = {
          Bold = false;
          Color = base0E;
        };
        Color6 = {
          Bold = false;
          Color = base0C;
        };
        Color7 = {
          Bold = false;
          Color = base07;
        };
        Foreground = {
          Bold = false;
          Color = base05;
        };
        Color0Faint = {
          Bold = false;
          Color = dim base00;
        };
        Color1Faint = {
          Bold = false;
          Color = dim base08;
        };
        Color2Faint = {
          Bold = false;
          Color = dim base0B;
        };
        Color3Faint = {
          Bold = false;
          Color = dim base0A;
        };
        Color4Faint = {
          Bold = false;
          Color = dim base0D;
        };
        Color5Faint = {
          Bold = false;
          Color = dim base0E;
        };
        Color6Faint = {
          Bold = false;
          Color = dim base0C;
        };
        Color7Faint = {
          Bold = false;
          Color = dim base07;
        };
        ForegroundFaint = {
          Bold = false;
          Color = dim base05;
        };
        Color0Intense = {
          Bold = false;
          Color = base00;
        };
        Color1Intense = {
          Bold = false;
          Color = base08;
        };
        Color2Intense = {
          Bold = false;
          Color = base0B;
        };
        Color3Intense = {
          Bold = false;
          Color = base0A;
        };
        Color4Intense = {
          Bold = false;
          Color = base0D;
        };
        Color5Intense = {
          Bold = false;
          Color = base0E;
        };
        Color6Intense = {
          Bold = false;
          Color = base0C;
        };
        Color7Intense = {
          Bold = false;
          Color = base07;
        };
        ForegroundIntense = {
          Bold = false;
          Color = base05;
        };
      };
  };
  home-manager.users.balsoft = {
    xdg.configFile.qmlkonsolerc.text = lib.generators.toGitINI {
      General = {
        colorScheme = "Generated";
        fontFamily = config.themes.fonts.mono.family;
        fontSize = config.themes.fonts.mono.size;
      };
    };
  };
}
