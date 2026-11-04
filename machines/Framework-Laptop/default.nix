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
      cores = 16;
    };
    drive = {
      type = "ssd";
      speed = 6000;
      size = 1024;
    };
    ram = 32;
  };

  fileSystems."/persist" = {
    device = "/dev/nvme0n1p2";
    fsType = "ext4";
    neededForBoot = true;
  };
  fileSystems."/boot" = {
    device = "/dev/nvme0n1p1";
    fsType = "vfat";
  };

  services.logind.settings.Login.HandleLidSwitch = "suspend";
  services.logind.settings.Login.HandlePowerKey = lib.mkForce "hibernate";
  services.logind.settings.Login.HandlePowerKeyLongPress = "poweroff";

  persist = {
    enable = true;
    cache.clean.enable = true;
  };

  nix.settings.max-jobs = lib.mkDefault 16;
  hardware.enableRedistributableFirmware = true;

  hardware.graphics = {
    enable = true;
    extraPackages = [
      pkgs.intel-media-driver
      pkgs.intel-vaapi-driver
      pkgs.vpl-gpu-rt
    ];
  };

  home-manager.users.balsoft.wayland.windowManager.sway.config = {
    input."*".natural_scroll = "enabled";
    output = {
      eDP-1.mode = "2880x1920@60Hz"; # 60 Hz to save a bit of power
    };
  };
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
