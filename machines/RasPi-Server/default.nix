{ inputs, pkgs, lib, ... }: {
  imports = with inputs.self.nixosModules; with inputs.self.nixosProfiles; [
    ./hardware-configuration.nix
    inputs.self.nixosRoles.server
    themes
    fonts
    cage
    gtk
    alacritty
  ];

  boot.loader.raspberryPi = {
    enable = true;
    version = 3;
  };

  nix.package = lib.mkForce pkgs.nixUnstable;

  deviceSpecific.devInfo = {
    cpu = {
      vendor = "broadcom";
      clock = 4200;
      cores = 8;
    };
    drive = {
      type = "ssd";
      speed = 6000;
      size = 250;
    };
    bigScreen = true;
    ram = 32;
  };
}
