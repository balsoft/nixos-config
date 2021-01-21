{ inputs, ... }: {
  imports = with inputs.self.nixosModules; [
    ./hardware-configuration.nix
    inputs.self.nixosProfiles.server
    mailserver
  ];

  boot.loader.raspberryPi = {
    enable = true;
    version = 3;
  };

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
