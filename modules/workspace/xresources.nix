{ config, lib, pkgs, ... }:
{
  home-manager.users.balsoft.xresources.properties = with config.themes.colors; {
    "*background" = bg;
    "*foreground" = fg;
    "*color0" = dark;
    "*color1" = alt;
    "*color2" = red;
    "*color3" = red;
    "*color4" = green;
    "*color5" = green;
    "*color6" = yellow;
    "*color7" = yellow;
    "*color8" = blue;
    "*color9" = blue;
    "*color10" = purple;
    "*color11" = purple;
    "*color12" = cyan;
    "*color13" = cyan;
    "*color14" = fg;
    "*color15" = fg;
  };
}
