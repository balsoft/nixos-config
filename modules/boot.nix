{ lib, pkgs, config, ... }: {
  boot = {
    loader.timeout = 1;

    loader.systemd-boot.enable = lib.mkIf (pkgs.system == "x86_64-linux") true;

    loader.grub.enable = false;

    kernelParams = [ "quiet" "scsi_mod.use_blk_mq=1" "modeset" "nofb" ]
      ++ lib.optional (pkgs.system == "x86_64-linux") [
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
