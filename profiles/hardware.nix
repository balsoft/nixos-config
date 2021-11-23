{
  hardware.enableRedistributableFirmware = true; # For some unfree drivers
  systemd.services.systemd-udev-settle.enable = false;
  services.fwupd.enable = true;
  # sound.enable = true;
}
