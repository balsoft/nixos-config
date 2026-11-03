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
    device = "/dev/disk/by-uuid/5143bdf2-7ba9-45bf-bc89-18e6bf282b07";
    fsType = "ext4";
    neededForBoot = true;
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/693D-03B8";
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
}
