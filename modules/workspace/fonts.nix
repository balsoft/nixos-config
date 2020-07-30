{ pkgs, config, lib, ... }:
{
  fonts = {
    fonts = with pkgs; [
      ibm-plex
      hasklig
      nerdfonts
      material-design-icons
      material-icons
    ];
    fontconfig = {
      enable = true;
      defaultFonts = {
        monospace = [ "IBM Plex Mono 13" ];
        sansSerif = [ "IBM Plex Sans 13" ];
        serif = [ "IBM Plex Serif 13" ];
      };
    };
    enableDefaultFonts = true;
  };
}
