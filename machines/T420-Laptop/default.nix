{ inputs, lib, ... }: {
  imports = with inputs.self.nixosModules; with inputs.self.nixosProfiles; [
    ./hardware-configuration.nix
    inputs.self.nixosRoles.server
    gitea
    # jitsi
    mailserver
    matrix-synapse
    # minidlna
    nextcloud
    nginx
    vsftpd
    # home-assistant
    # mastodon
    irc
    remapper
    ntfy
  ];

  services.logind.lidSwitch = "ignore";

  system.stateVersion = "21.11";

  boot.loader = {
    systemd-boot.enable = lib.mkForce false;
    grub = {
      enable = lib.mkForce true;
      device = "/dev/disk/by-path/pci-0000:06:00.0-scsi-0:0:0:0";
    };
  };

  deviceSpecific.devInfo = {
    legacy = true;
    cpu = {
      vendor = "intel";
      clock = 2500;
      cores = 2;
    };
    drive = {
      type = "ssd";
      speed = 1000;
      size = 120;
    };
    ram = 8;
  };
}
