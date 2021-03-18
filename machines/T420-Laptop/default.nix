{ inputs, ... }: {
  imports = with inputs.self.nixosModules; [
    ./hardware-configuration.nix
    inputs.self.nixosProfiles.base
    # gitea
    # jitsi
    mailserver
    matrix-synapse
    minidlna
    nextcloud
    nginx
    vsftpd
    home-assistant
    mastodon
  ];

  services.logind.lidSwitch = "ignore";

  security.sudo.wheelNeedsPassword = false;

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
