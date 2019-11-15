{ pkgs, config, lib, ... }:

with rec { inherit (config) device devices deviceSpecific; };
with deviceSpecific; {

  hardware.sensor.iio.enable = (device == "HP-Laptop");
  hardware.cpu.${devices.${device}.cpu.vendor}.updateMicrocode =
    true; # Update microcode

  hardware.enableRedistributableFirmware = true; # For some unfree drivers

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.driSupport32Bit = true; # For steam

  hardware.bluetooth.enable = true;
  hardware.bluetooth.package = pkgs.bluezFull;

  hardware.sane.enable = true;

  services.throttled = {
    enable = device == "ThinkPad-Laptop";
    extraConfig = ''
      [GENERAL]
      # Enable or disable the script execution
      Enabled: True
      # SYSFS path for checking if the system is running on AC power
      Sysfs_Power_Path: /sys/class/power_supply/AC*/online

      ## Settings to apply while connected to Battery power
      [BATTERY]
      # Update the registers every this many seconds
      Update_Rate_s: 30
      # Max package power for time window #1
      PL1_Tdp_W: 29
      # Time window #1 duration
      PL1_Duration_s: 28
      # Max package power for time window #2
      PL2_Tdp_W: 44
      # Time window #2 duration
      PL2_Duration_S: 0.002
      # Max allowed temperature before throttling
      Trip_Temp_C: 85
      # Set cTDP to normal=0, down=1 or up=2 (EXPERIMENTAL)
      cTDP: 1

      ## Settings to apply while connected to AC power
      [AC]
      # Update the registers every this many seconds
      Update_Rate_s: 5
      # Max package power for time window #1
      PL1_Tdp_W: 44
      # Time window #1 duration
      PL1_Duration_s: 28
      # Max package power for time window #2
      PL2_Tdp_W: 44
      # Time window #2 duration
      PL2_Duration_S: 0.002
      # Max allowed temperature before throttling
      Trip_Temp_C: 95
      # Set HWP energy performance hints to 'performance' on high load (EXPERIMENTAL)
      HWP_Mode: True
      # Set cTDP to normal=0, down=1 or up=2 (EXPERIMENTAL)
      cTDP: 2

      [UNDERVOLT]
      # CPU core voltage offset (mV)
      CORE: -60
      # Integrated GPU voltage offset (mV)
      GPU: -50
      # CPU cache voltage offset (mV)
      CACHE: -30
      # System Agent voltage offset (mV)
      UNCORE: 0
      # Analog I/O voltage offset (mV)
      ANALOGIO: 0
    '';
  };

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
    blacklistedKernelModules = lib.optionals (device == "Prestigio-Laptop") [
      "axp288_charger"
      "axp288_fuel_gauge"
      "axp288_adc"
    ]; # Disable battery driver as it hangs this piece of shit
    extraModulePackages = [ pkgs.linuxPackages.v4l2loopback ];
    extraModprobeConfig = if (device == "ASUS-Laptop") then
      "options iwlwifi swcrypto=1 power_save=0 power_level=5 11n_disable=8 bt_coex_active=1"
    else
      ""; # Attempt to fix broken wireless
    kernel.sysctl."vm.swappiness" = 0;
    kernelPackages = pkgs.linuxPackages;
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
    # systemWide = true;
    extraConfig = ''
      load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1
    '';
    extraModules = [ pkgs.pulseaudio-modules-bt ];
  };

  environment.etc.fancontrol = {
    enable = device == "AMD-Workstation";
    text = ''
      INTERVAL=3
      DEVPATH=hwmon1=devices/pci0000:00/0000:00:03.1/0000:26:00.0
      DEVNAME=hwmon1=amdgpu
      FCTEMPS=hwmon1/pwm1=hwmon1/temp1_input
      FCFANS= hwmon1/pwm1=
      MINTEMP=hwmon1/pwm1=20
      MAXTEMP=hwmon1/pwm1=70
      MINSTART=hwmon1/pwm1=255
      MINSTOP=hwmon1/pwm1=0
    '';
  };

  services.dbus.packages = [ pkgs.blueman ];

  systemd.services.fancontrol = {
    enable = device == "AMD-Workstation";
    description = "Control the speed of fans";
    script = "sleep 10; ${pkgs.lm_sensors}/bin/fancontrol";
    serviceConfig.User = "root";
    wantedBy = [ "multi-user.target" ];
  };
}
