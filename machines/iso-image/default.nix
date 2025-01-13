{ modulesPath, lib, inputs, pkgs, ... }: {
  imports = with inputs.self.nixosModules; with inputs.self.nixosProfiles; [
    "${modulesPath}/installer/cd-dvd/iso-image.nix"
    inputs.self.nixosRoles.base
    themes
    fonts
    cage
    gtk
    alacritty
  ];
  nix.settings.max-jobs = 4;
  isoImage.makeEfiBootable = true;
  isoImage.makeUsbBootable = true;
}
