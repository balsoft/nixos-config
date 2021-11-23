{ lib, inputs, pkgs, config, ... }: {
  imports = with inputs.self.nixosModules; with inputs.self.nixosProfiles; [
    ./hardware-configuration.nix

    inputs.self.nixosRoles.base

    themes
    fonts
    gtk

    sway
    i3blocks
    himalaya
    bluetooth

    simple-osd-daemons
    alacritty
    firefox
    emacs
  ];

  deviceSpecific.devInfo = {
    cpu = {
      vendor = "intel";
      clock = 2000;
      cores = 2;
    };
    drive = {
      type = "ssd";
      speed = 6000;
      size = 16;
    };
    bigScreen = false;
    ram = 2;
  };

  networking.wireless.enable = lib.mkForce false;
  services.openssh.permitRootLogin = lib.mkForce "no";
  services.getty.autologinUser = lib.mkForce "balsoft";
  boot.kernelPackages = lib.mkForce pkgs.linuxPackages_latest;
  boot.supportedFilesystems = lib.mkForce [ "ext4" "vfat" ];
  boot.loader.systemd-boot.enable = lib.mkForce false;
  boot.loader.grub = {
    enable = lib.mkForce true;
    devices = [ "/dev/sdc" ];
  };
  persist = {
    enable = true;
  };

  defaultApplications = {
    monitor.cmd = "${pkgs.alacritty}/bin/alacritty -e top";
  };

  startupApplications = [ config.defaultApplications.browser.cmd ];

}
