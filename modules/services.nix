
{ config, lib, pkgs, ... }: {

  services.acpid.enable = true;

  services.earlyoom = {
    enable = config.deviceSpecific.devInfo.ram < 16;
    freeMemThreshold = 5;
    freeSwapThreshold = 100;
  };

  services.printing = {
    enable = true;
    drivers = [ pkgs.gutenprint ];
  };

  programs.mosh.enable = true;

  services.fwupd.enable = true;

  services.avahi.enable = true;

  systemd.services.systemd-udev-settle.enable = false;

  services.pcscd.enable = true;

  services.upower.enable = true;
}
