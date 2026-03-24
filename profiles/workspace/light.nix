{ config, pkgs, lib, ... }: {
  environment.systemPackages = lib.mkIf config.deviceSpecific.isLaptop [
    pkgs.brightnessctl
  ];
  services.actkbd = {
    enable = config.deviceSpecific.isLaptop;
    bindings = map (x:
      x // {
        events = [ "key" ];
        attributes = [ "exec" ];
      }) [
        {
          keys = [ 225 ];
          command = "${lib.getExe pkgs.brightnessctl} s '10%+'";
        }
        {
          keys = [ 224 ];
          command = "${lib.getExe pkgs.brightnessctl} s '10%-'";
        }
      ];
  };
}
