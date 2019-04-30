{pkgs, config, lib, ...}:

with rec {
  inherit (config) device devices deviceSpecific;
};
with deviceSpecific;
{

  hardware.sensor.iio.enable = (device == "HP-Laptop");
  hardware.cpu.${devices.${device}.cpu.vendor}.updateMicrocode = true; # Update microcode

  hardware.enableRedistributableFirmware = true; # For some unfree drivers

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true; # For steam

  hardware.bluetooth.enable = true;
  hardware.sane.enable = true;

  boot = {
    loader = {
      grub.enable = true;
      grub.version = 2;
      grub.useOSProber = true;
      timeout = 1;
    } // (if device == "Lenovo-Workstation" then { # Non-UEFI config
      grub.device = "/dev/sda";
    } else { # UEFI config
      grub.efiSupport = true;
      grub.device = "nodev";
      grub.efiInstallAsRemovable = true; # NVRAM is unreliable
    });
    consoleLogLevel = 3;
    blacklistedKernelModules = lib.optionals (device == "Prestigio-Laptop") [ "axp288_charger" "axp288_fuel_gauge" "axp288_adc" ]; # Disable battery driver as it hangs this piece of shit
    extraModprobeConfig = if (device == "ASUS-Laptop") then "options iwlwifi swcrypto=1 power_save=0 power_level=5 11n_disable=8 bt_coex_active=1" else ""; # Attempt to fix broken wireless
    kernel.sysctl."vm.swappiness" = 0;
    kernelPackages = pkgs.linuxPackages_latest;
    kernelParams = [ 
      "quiet" 
      "scsi_mod.use_blk_mq=1" 
      "modeset" 
      "nofb" 
      "rd.systemd.show_status=auto" 
      "rd.udev.log_priority=3" 
      "pti=off" 
      "spectre_v2=off"
    ] ++ lib.optionals (device == "Prestigio-Laptop") [
      "intel_idle.max_cstate=1" # Otherwise it hangs
    ];
  };

  services.logind.extraConfig = "HandlePowerKey=suspend";
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;  
    support32Bit = true;
  };
}
