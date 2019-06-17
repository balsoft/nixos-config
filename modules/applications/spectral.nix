{ pkgs, config, lib, ... }: {
  nixpkgs.overlays = [
    (self: super: {
      spectral = super.spectral.overrideAttrs (oldAttrs: {
        prePatch =
        "substituteInPlace qml/main.qml --replace '#303030' '${config.themes.colors.bg}'";
      });
    })
  ];
}
