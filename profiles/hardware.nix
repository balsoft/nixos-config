{ pkgs, config, lib, inputs, ... }:

with rec { inherit (config) device deviceSpecific; };
with deviceSpecific; {
  hardware.enableRedistributableFirmware = true; # For some unfree drivers

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true; # For steam
  hardware.opengl.package = pkgs.mesa_drivers;

  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluezFull;
  };

  systemd.services.bluetooth.serviceConfig.ExecStart = lib.mkForce [ "" "${pkgs.bluezFull}/libexec/bluetooth/bluetoothd -f /etc/bluetooth/main.conf -E" ];

  persist.state.directories = [ "/var/lib/bluetooth" ];

  systemd.services.systemd-udev-settle.enable = false;

  services.upower = { enable = true; };

  services.logind.lidSwitchExternalPower = "ignore";

  services.logind.extraConfig = "HandlePowerKey=suspend";

  services.fwupd.enable = true;
  # sound.enable = true;
}
