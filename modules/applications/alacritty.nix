{ config, pkgs, lib, ... }:
let thm = config.themes.colors;
in {
  home-manager.users.balsoft.programs.alacritty = {
    enable = true;
    settings = {

      font = rec {
        normal.family = "IBM Plex Mono";
        size = 11;
        bold = { style = "Bold"; };
      };

      window.padding = {
        x = 2;
        y = 2;
      };

      shell.program = "${pkgs.zsh}/bin/zsh";

      cursor.style = "Beam";

      colors = {
        primary = {
          background = thm.bg;
          foreground = thm.fg;
        };
        cursor = {
          text = thm.alt;
          cursor = thm.fg;
        };
        normal = {
          black = thm.bg;
          inherit (thm) red green yellow blue cyan;
          magenta = thm.purple;
          white = thm.fg;
        };
      };
    };
  };

}
