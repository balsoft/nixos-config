{
  inputs,
  lib,
  config,
  pkgs,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
    inputs.self.nixosRoles.desktop
    inputs.self.nixosProfiles.fprintd
  ];
  deviceSpecific.devInfo = {
    cpu = {
      vendor = "intel";
      clock = 4800;
      cores = 16;
    };
    drive = {
      type = "ssd";
      speed = 6000;
      size = 1024;
    };
    ram = 32;
  };

  fileSystems."/persist" = {
    device = "/dev/nvme0n1p2";
    fsType = "ext4";
    neededForBoot = true;
  };
  fileSystems."/boot" = {
    device = "/dev/nvme0n1p1";
    fsType = "vfat";
  };

  # services.logind.settings.Login.HandleLidSwitch = "suspend";
  # services.logind.settings.Login.HandlePowerKey = "poweroff";

  persist = {
    enable = true;
    cache.clean.enable = true;
  };

  nix.settings.max-jobs = lib.mkDefault 16;
  hardware.enableRedistributableFirmware = true;

  hardware.graphics = {
    enable = true;
    extraPackages = [
      pkgs.intel-media-driver
      pkgs.intel-vaapi-driver
      pkgs.vpl-gpu-rt
    ];
  };

  environment.systemPackages = [ config.boot.kernelPackages.cpupower ];

  environment.variables = {
    LIBVA_DRIVER_NAME = "iHD";
    ANV_DEBUG = "video-decode,video-encode";
  };

  home-manager.users.balsoft = {
    wayland.windowManager.sway.config = {
      input."*".natural_scroll = "enabled";
      output = {
        eDP-1.mode = "2880x1920@60Hz"; # 60 Hz by default to save a bit of power
      };
    };
    xdg.configFile."simple-osd/brightness".text = pkgs.my-lib.genIni {
      default = {
        "backlight backend" = "/sys/class/backlight/acpi_video0";
        "refresh interval" = 1000;
      };
    };
    wayland.windowManager.sway.config.output.eDP-1.scale = "1.75";
  };

  powerManagement.powertop.enable = true;

  systemd.services =
    let
      sudo = "/run/wrappers/bin/sudo -u balsoft";
      swaymsg = ''${sudo} SWAYSOCK="$(echo /run/user/1000/sway-ipc.*.sock)" ${pkgs.sway}/bin/swaymsg'';
      cpupower = "${config.boot.kernelPackages.cpupower}/bin/cpupower";
      # Apply a "histeresis" to the output mode switching to prevent blinking when the battery switches between "fully charged" and "charging"
      histeresis = 5;
      mkUnit =
        {
          description,
          hz,
          adaptiveSync,
          governor,
          bias,
          epp,
          turbo,
          conflicts,
        }:
        {
          inherit description conflicts;
          serviceConfig.RemainAfterExit = true;
          script = ''
            sleep ${toString histeresis}
            ${swaymsg} "output eDP-1 mode 2880x1920@${toString hz}Hz adaptive_sync ${
              if adaptiveSync then "yes" else "no"
            }"
            ${cpupower} frequency-set -g ${governor}
            ${cpupower} set -b ${toString bias} -e ${epp} -t ${if turbo then "1" else "0"}
          '';
        };
    in
    {
      powersave = mkUnit {
        description = "Save us much power as possible";
        hz = 60;
        adaptiveSync = false;
        governor = "powersave";
        bias = 15;
        epp = "power";
        turbo = false;
        conflicts = [
          "performance.service"
          "balanced.service"
        ];
      };

      balanced = mkUnit {
        description = "Balanced power profile";
        hz = 60;
        adaptiveSync = false;
        governor = "powersave";
        bias = 7;
        epp = "balance_power";
        turbo = true;
        conflicts = [
          "performance.service"
          "powersave.service"
        ];
      };

      performance = mkUnit {
        description = "Maximum power!";
        hz = 120;
        adaptiveSync = true;
        governor = "performance";
        bias = 0;
        epp = "performance";
        turbo = true;
        conflicts = [
          "powersave.service"
          "balanced.service"
        ];
      };
    };

  services.udev.extraRules =
    let
      systemctl = "${pkgs.systemd}/bin/systemctl";
    in
    ''
      SUBSYSTEM=="power_supply",ENV{POWER_SUPPLY_ONLINE}=="0",RUN+="${systemctl} start powersave"
      SUBSYSTEM=="power_supply",ENV{POWER_SUPPLY_ONLINE}=="1",RUN+="${systemctl} start performance"
    '';
}
