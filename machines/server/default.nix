{ inputs, lib, config, ... }:
{
  imports =
    with inputs.self.nixosModules;
    with inputs.self.nixosProfiles;
    [
      inputs.self.nixosRoles.base
      helix
      direnv
      kitty
    ];
  home-manager.users.balsoft = {
    home.uid = lib.mkForce 1024;

  };
  home-manager.backupFileExtension = ".hm-bkp";

  nix.nrBuildUsers = lib.mkForce 16;

  deviceSpecific.devInfo = {
    legacy = true;
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
