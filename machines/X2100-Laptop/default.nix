{
  inputs,
  lib,
  config,
  pkgs,
  ...
}:
{
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

  fileSystems."/persist" = {
    device = "/dev/disk/by-uuid/e50bd1d6-3613-465e-895a-9dde6ffaad46";
    fsType = "ext4";
    neededForBoot = true;
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/621A-6414";
    fsType = "vfat";
  };

  boot.resumeDevice = config.fileSystems."/persist".device;
  boot.kernelParams = [ "resume_offset=4294656" ]; # from sudo filefrag -v /persist/swapfile
  services.logind.lidSwitch = "suspend";
  services.logind.powerKey = lib.mkForce "hibernate";
  services.logind.powerKeyLongPress = "poweroff";

  # boot.initrd.systemd.enable = true;

  swapDevices = [
    {
      device = "/persist/swapfile";
      size = 16 * 1024; # 16 GB
    }
  ];

  persist = {
    enable = true;
    cache.clean.enable = true;
  };

  nix.settings.max-jobs = lib.mkDefault 8;

  home-manager.users.balsoft = {
    xdg.configFile."simple-osd/brightness".text = pkgs.my-lib.genIni {
      default = {
        "backlight backend" = "/sys/class/backlight/acpi_video0";
        "refresh interval" = 1000;
      };
    };
    wayland.windowManager.sway.config.output.eDP-1.scale = "1.9";
  };
}
