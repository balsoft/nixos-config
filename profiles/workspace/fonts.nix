{ pkgs, config, lib, ... }: {
  fonts = {
    packages = with pkgs; [
      ibm-plex
      nerdfonts
      material-design-icons
      material-icons
      fira-code
      fira-code-symbols
    ];
    fontconfig = let fonts = config.themes.fonts;
    in {
      enable = lib.mkForce true;
      defaultFonts = {
        monospace = [ "${fonts.mono.family} ${toString fonts.mono.size}" ];
        sansSerif = [ "${fonts.main.family} ${toString fonts.main.size}" ];
        serif = [ "${fonts.serif.family} ${toString fonts.serif.size}" ];
      };
    };
    enableDefaultPackages = true;
  };
  themes.fonts = {
    main = {
      family = "IBM Plex Sans";
      size = lib.mkDefault 13;
    };
    serif = {
      family = "IBM Plex Serif";
      size = lib.mkDefault 13;
    };
    mono = {
      family = "IBM Plex Mono";
      size = lib.mkDefault 13;
    };
  };
}
