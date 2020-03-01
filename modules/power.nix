{ config, pkgs, lib, ... }:

with rec {
  inherit (config) device devices deviceSpecific;
};
with deviceSpecific; {
  services.udev.extraRules = if isLaptop then
    ''
      ACTION=="add|change", KERNEL=="sd*[!0-9]|sr*", ATTR{queue/scheduler}="bfq"
      ACTION=="change", SUBSYSTEM=="power_supply", ATTR{online}=="0", RUN+="${
        pkgs.systemd
      }/bin/systemctl start battery"
        ACTION=="change", SUBSYSTEM=="power_supply", ATTR{online}=="1", RUN+="${
        pkgs.systemd
      }/bin/systemctl start ac"
        ACTION=="add|change", SUBSYSTEM=="backlight", MODE:="0777"
    '' + (if device == "ASUS-Laptop" then ''
      ACTION=="add|change", SUBSYSTEM=="net", KERNEL=="wlan*" RUN+="${
        pkgs.iw
      }/bin/iw dev %k set power_save off"
    '' else
      "")
  else
    "";
  systemd.services.battery = {
    enable = isLaptop;
    description = "Executes commands needed on battery power";
    script = ''
            ${
        pkgs.linuxPackages_latest.cpupower
      }/bin/cpupower frequency-set -g powersave
            ${pkgs.hdparm}/bin/hdparm -B 1 /dev/sda
      echo "500" > /sys/class/backlight/*/brightness
        '';
  };
  systemd.services.ac = {
    enable = isLaptop;
    description = "Executes commands needed on ac power";
    script = ''
            ${
        pkgs.linuxPackages_latest.cpupower
      }/bin/cpupower frequency-set -g performance
            ${pkgs.hdparm}/bin/hdparm -B 255 /dev/sda
      echo "900" > /sys/class/backlight/*/brightness
        '';
  };
}
