{ pkgs, config, lib, ... }:
with import ../../support.nix { inherit lib config; }; {
  nixpkgs.overlays = [
    (self: super: {
      spectral = super.spectral.overrideAttrs (oldAttrs: {
        prePatch = ''
          substituteInPlace qml/main.qml --replace "#303030" "${config.themes.colors.bg}"
          cp ${
            pkgs.writeTextFile {
              name = "qtquickcontrols2.conf";
              text = genIni {
                Controls.Style = "Material";
                Material = {
                  Theme = "Dark";
                  Variant = "Dense";
                  Primary = config.themes.colors.alt;
                  Accent = config.themes.colors.gray;
                  Background = config.themes.colors.bg;
                  Foreground = config.themes.colors.fg;
                };
              };
            }
          } .
        '';
      });
    })
  ];
  home-manager.users.balsoft = {
    xsession.windowManager.i3.config = {
      startup = [{ command = "${pkgs.spectral}/bin/spectral"; }];
    };
    home.packages = [ pkgs.spectral ];
  };
}
