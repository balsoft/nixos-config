{ pkgs, config, lib, ... }: {
  nixpkgs.overlays = [
    (self: super: {
      spectral = super.spectral.overrideAttrs (oldAttrs: {
        prePatch =
        "substituteInPlace qml/main.qml --replace '#303030' '${config.themes.colors.bg}'";
      });
    })
  ];
  home-manager.users.balsoft = {
    xsession.windowManager.i3.config.startup =
    [{ command = "${pkgs.spectral}/bin/spectral"; }];
    home.packages = [ pkgs.spectral ];
  };
}
