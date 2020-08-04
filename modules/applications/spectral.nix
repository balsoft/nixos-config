{ pkgs, config, lib, ... }:
with import ../../support.nix { inherit lib config; }; {
  nixpkgs.overlays = [
    (self: super: {
      spectral = super.spectral.overrideAttrs (oldAttrs: {
        prePatch = ''
          sed -i \
          -e s/#303030/${config.themes.colors.bg}/ \
          -e s/#009DC2/${config.themes.colors.alt}/ \
          -e s/#673AB7/${config.themes.colors.alt}/ \
          -e s/#4285F4/${config.themes.colors.gray}/ \
          -e s/#242424/${config.themes.colors.bg}/ \
          -e 's/"#ff2b00", "#ff5500", "#ff8000", "#ffaa00", "#ffd500", "#ffff00", "#d4ff00", "#aaff00", "#80ff00", "#55ff00", "#2bff00", "#00ff00", "#00ff2b", "#00ff55", "#00ff80", "#00ffaa", "#00ffd5", "#00ffff", "#00d4ff", "#00aaff", "#007fff", "#0055ff", "#002bff", "#0000ff", "#2a00ff", "#5500ff", "#7f00ff", "#aa00ff", "#d400ff", "#ff00ff", "#ff00d4", "#ff00aa", "#ff0080", "#ff0055", "#ff002b", "#ff0000"/"${config.themes.colors.alt}"/' \
          $(find . -name "*.qml")
        '';
      });
    })
  ];
}
