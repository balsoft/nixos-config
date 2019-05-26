{ config, lib, pkgs, ... }:
{
  home-manager.users.balsoft = {
    xresources.properties = with config.themes.colors; {
      "*background" = bg;
      "*foreground" = fg;
      "*color0" = dark;
      "*color1" = green;
      "*color2" = green;
      "*color3" = yellow;
      "*color4" = fg;
      "*color5" = purple;
      "*color6" = cyan;
      "*color7" = gray;
      "*color8" = alt;
      "*color9" = green;
      "*color10" = green;
      "*color11" = yellow;
      "*color12" = green;
      "*color13" = purple;
      "*color14" = cyan;
      "*color15" = fg;
    };
    home.activation.xrdb = {
      after = ["linkGeneration"];
      before = [];
      data = "DISPLAY=:0 ${pkgs.xorg.xrdb}/bin/xrdb -merge .Xresources";
    };
  };
}
