{ config, lib, pkgs, ... }:
{
  home-manager.users.balsoft.xresources.properties = with config.themes.colors; {
    "*background" = bg;
    "*foreground" = fg;
    "*color0" = dark;
    "*color1" = red;
    "*color2" = green;
    "*color3" = yellow;
    "*color4" = blue;
    "*color5" = purple;
    "*color6" = cyan;
    "*color7" = gray;
    "*color8" = alt;
    "*color9" = red;
    "*color10" = green;
    "*color11" = yellow;
    "*color12" = blue;
    "*color13" = purple;
    "*color14" = cyan;
    "*color15" = fg;
  };
}
