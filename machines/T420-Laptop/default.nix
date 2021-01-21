{ inputs, ... }: {
  imports = [ ./hardware-configuration.nix inputs.self.nixosProfiles.desktop ];
  deviceSpecific.devInfo = {
    legacy = false;
    cpu = {
      vendor = "intel";
      clock = 2500;
      cores = 2;
    };
    drive = {
      type = "ssd";
      speed = 1000;
      size = 120;
    };
    ram = 8;
  };
}
