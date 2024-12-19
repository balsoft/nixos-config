{ config, pkgs, ... }: {
  defaultApplications.term = {
    cmd = "${pkgs.alacritty}/bin/alacritty";
    desktop = "alacritty";
  };
  home-manager.users.balsoft.programs.alacritty = {
    enable = true;
    settings = {
      keyboard.bindings = [{
        key = "Return";
        mods = "Control";
        action = "SpawnNewInstance";
      }];

      font = {
        normal.family = config.themes.fonts.mono.family;
        size = config.themes.fonts.mono.size;
        bold = { style = "Bold"; };
      };

      window.padding = {
        x = 2;
        y = 2;
      };

      terminal.shell.program = "${pkgs.zsh}/bin/zsh";

      cursor.style = "Beam";

      colors = with pkgs.my-lib.thmHash config.themes.colors; {
        primary = {
          background = base00;
          foreground = base05;
          dim_foreground = base04;
        };
        cursor = {
          text = base02;
          cursor = base05;
        };
        normal = {
          black = base00;
          red = base08;
          green = base0B;
          yellow = base0A;
          blue = base0D;
          magenta = base0E;
          white = base07;
        };
      };
    };
  };

}
