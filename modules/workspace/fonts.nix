{ pkgs, config, lib, ... }: {
  fonts = {
    fonts = with pkgs; [
      terminus_font
      opensans-ttf
      roboto
      roboto-mono
      roboto-slab
      nerdfonts
      noto-fonts
      noto-fonts-emoji
      hasklig
      material-design-icons
      material-icons
    ];
    fontconfig = {
      enable = true;
      defaultFonts = {
        monospace = ["Roboto Mono 13"];
        sansSerif = ["Roboto 13"];
        serif = ["Roboto Slab 13"];
      };
    };
    enableDefaultFonts = true;
  };
}
