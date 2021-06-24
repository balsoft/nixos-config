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

  persist = {
    enable = true;
    cache.clean.enable = true;
  };

  home-manager.users.balsoft.xdg.configFile."simple-osd/brightness".text =
    pkgs.my-lib.genIni {
      default = {
        "backlight backend" = "/sys/class/backlight/intel_backlight";
        "refresh interval" = 100;
      };
    };
  boot.extraModprobeConfig = ''
    options iwlwifi bt_coex_active=0
  '';
}
