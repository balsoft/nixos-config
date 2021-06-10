{ config, lib, pkgs, ... }:
{
  home-manager.users.balsoft = {
    xresources.properties = with pkgs.my-lib.thmHash; {
      "*background" = base00;
      "*foreground" = base07;
      "*color0" = base00;
      "*color1" = base08;
      "*color2" = base0B;
      "*color3" = base0A;
      "*color4" = base0D;
      "*color5" = base0E;
      "*color6" = base0C;
      "*color7" = base07;
      "*color8" = base03;
      "*color9" = base09;
      "*color10" = base01;
      "*color11" = base02;
      "*color12" = base04;
      "*color13" = base07;
      "*color14" = base0F;
      "*color15" = base07;

      "emacs.font" = "${config.themes.fonts.mono.family} ${toString config.themes.fonts.mono.size}";
      
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
  };
}
