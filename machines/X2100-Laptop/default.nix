{ inputs, lib, config, ... }: {
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
  home-manager.users.balsoft.xdg.configFile."simple-osd/brightness".text = (import ../../support.nix { inherit lib config; }).genIni {
    default = {
      "backlight backend" = "/sys/class/backlight/intel_backlight";
      "refresh interval" = 100;
    };
  };
  boot.extraModprobeConfig = ''
    options iwlwifi bt_coex_active=0
  '';
}
