{ config, lib, pkgs, ... }:
{
  home-manager.users.balsoft = {
    xresources.properties = with config.themes.colors; {
      "*background" = bg;
      "*foreground" = fg;
      "*color0" = dark;
      "*color1" = red;
      "*color2" = green;
      "*color3" = yellow;
      "*color4" = blue;
      "*color5" = purple;
      "*color6" = cyan;
      "*color7" = fg;
      "*color8" = dark;
      "*color9" = red;
      "*color10" = green;
      "*color11" = yellow;
      "*color12" = blue;
      "*color13" = purple;
      "*color14" = cyan;
      "*color15" = fg;
      
      # "emacs.color0" = dark;
      # "emacs.color1" = green;
      # "emacs.color2" = alt;
      # "emacs.color3" = yellow;
      # "emacs.color4" = gray;
      # "emacs.color5" = purple;
      # "emacs.color6" = cyan;
      # "emacs.color7" = gray;
      # "emacs.color8" = alt;
      # "emacs.color9" = green;
      # "emacs.color10" = green;
      # "emacs.color11" = yellow;
      # "emacs.color12" = green;
      # "emacs.color13" = purple;
      # "emacs.color14" = cyan;
      # "emacs.color15" = fg;

    };
    home.activation.xrdb = {
      after = ["linkGeneration"];
      before = [];
      data = "DISPLAY=:0 ${pkgs.xorg.xrdb}/bin/xrdb -merge ${config.users.users.balsoft.home}/.Xresources";
    };
  };
}
