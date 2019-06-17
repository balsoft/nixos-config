{ pkgs, config, lib, ... }:
with import ../../support.nix { inherit lib config; }; {
  nixpkgs.overlays = [
    (self: super: {
      spectral = super.spectral.overrideAttrs (oldAttrs: {
        prePatch = ''
          substituteInPlace qml/main.qml --replace "#303030" "${config.themes.colors.bg}"
          substituteInPlace ./imports/Spectral/Component/Timeline/MessageDelegate.qml \
          --replace "#009DC2" "${config.themes.colors.alt}" \
          --replace "#673AB7" "${config.themes.colors.dark}" \
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
