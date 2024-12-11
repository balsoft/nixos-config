{ inputs, lib, config, pkgs, ... }: {
  imports = [
    ./hardware-configuration.nix
    inputs.self.nixosRoles.desktop
  ];
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
  
  fileSystems."/persist" =
    { device = "/dev/disk/by-uuid/e50bd1d6-3613-465e-895a-9dde6ffaad46";
      fsType = "ext4";
      neededForBoot = true;
    };
  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/621A-6414";
      fsType = "vfat";
    };

  persist = {
    enable = true;
    cache.clean.enable = true;
  };

  nix.settings.max-jobs = lib.mkDefault 8;

  home-manager.users.balsoft.xdg.configFile."simple-osd/brightness".text =
    pkgs.my-lib.genIni {
      default = {
        "backlight backend" = "/sys/class/backlight/acpi_video0";
        "refresh interval" = 1000;
      };
    };
}
