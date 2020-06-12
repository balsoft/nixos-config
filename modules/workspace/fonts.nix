{ pkgs, config, lib, ... }:
let
  plex = pkgs.runCommand "ibm-plex" {
    src = pkgs.fetchzip {
      url = "https://github.com/IBM/plex/releases/download/v5.0.0/TrueType.zip";
      sha256 =
        "sha256-KKw9pk5YmWpaMKnYKhjwHynHxx8c0F8U/fgoU9qimHY=";
    };
  }
    "mkdir -p $out/share/fonts/truetype; cp $src/**/*.ttf $out/share/fonts/truetype";
in {
  fonts = {
    fonts = with pkgs; [
      plex
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
