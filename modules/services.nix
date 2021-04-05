
{ config, lib, pkgs, inputs, ... }: {

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

  services.blueman.enable = true;

  services.upower = {
    enable = true;
    package = pkgs.upower.overrideAttrs (oa: {
      src = inputs.upower;
      nativeBuildInputs = oa.nativeBuildInputs ++ [ pkgs.autoreconfHook pkgs.gtk_doc ];
    });
  };
}
