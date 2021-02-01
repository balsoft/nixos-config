{ inputs, ... }: {
  imports = [
    ./hardware-configuration.nix
    inputs.self.nixosProfiles.desktop
  ];
  deviceSpecific.devInfo = {
    cpu = {
      vendor = "amd";
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
  deviceSpecific.isHost = true;
  services.apcupsd.enable = true;
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
}
