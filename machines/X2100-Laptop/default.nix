{ inputs, ... }: {
  imports = [ ./hardware-configuration.nix inputs.self.nixosProfiles.desktop ];
  deviceSpecific.devInfo = {
    cpu = {
      vendor = "intel";
      clock = 4800;
      cores = 4;
    };
    drive = {
      type = "ssd";
      speed = 6000;
      size = 256;
    };
    ram = 16;
  };
  boot.extraModprobeConfig = ''
    options iwlwifi bt_coex_active=0
  '';
}
