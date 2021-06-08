{ lib, pkgs, config, ... }: {
  boot = {
    loader = {
      timeout = lib.mkForce 1;
      grub.enable = lib.mkForce false;
      systemd-boot.enable = pkgs.system == "x86_64-linux";
    };
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
  persist.state.etcFiles = [ "machine-id" ];
}
