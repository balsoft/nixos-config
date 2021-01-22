{ lib, pkgs, config, ... }: {
  boot = {
    loader = {
      timeout = 1;
    } // (if config.deviceSpecific.devInfo.legacy or false then { # Non-UEFI config
      grub.enable = true;
      grub.version = 2;
      grub.useOSProber = true;
      grub.device = "/dev/sda";
    } else { # UEFI config
      systemd-boot.enable = true;
    });
    kernelParams = [ "quiet" "scsi_mod.use_blk_mq=1" "modeset" "nofb" ]
      ++ lib.optionals (pkgs.system == "x86_64-linux") [
        "rd.systemd.show_status=auto"
        "rd.udev.log_priority=3"
        "pti=off"
        "spectre_v2=off"
      ];

    kernelPackages = pkgs.linuxPackages_latest;

    consoleLogLevel = 3;
    kernel.sysctl."vm.swappiness" = 0;
    kernel.sysctl."kernel/sysrq" = 1;
  };
}
