{ lib, pkgs, config, ... }: {
  boot = {
    loader = {
      timeout = lib.mkForce 4;
      grub.enable = false;
      systemd-boot.enable = pkgs.system == "x86_64-linux";
    };
    kernelParams = [ "modeset" "nofb" "preempt=full" ]
      ++ lib.optionals (pkgs.system == "x86_64-linux") [
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
