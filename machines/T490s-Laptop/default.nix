{ inputs, ... }: {
  imports = [ ./hardware-configuration.nix inputs.self.nixosRoles.desktop inputs.self.nixosProfiles.print-scan ];
  deviceSpecific.devInfo = {
    cpu = {
      vendor = "intel";
      clock = 4600;
      cores = 4;
    };
    drive = {
      type = "ssd";
      speed = 2000;
      size = 250;
    };
    ram = 16;
  };

  persist = {
    enable = true;
    cache.clean.enable = true;
  };

  home-manager.users.balsoft.wayland.windowManager.sway.config.input."*".natural_scroll = "enable";

  services.throttled = {
    enable = true;
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
      CORE: -200
      # Integrated GPU voltage offset (mV)
      GPU: -60
      # CPU cache voltage offset (mV)
      CACHE: -50
      # System Agent voltage offset (mV)
      UNCORE: 0
      # Analog I/O voltage offset (mV)
      ANALOGIO: 0
    '';
  };

}
