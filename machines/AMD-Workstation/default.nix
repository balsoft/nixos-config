{ config, inputs, ... }: {
  imports = [
    ./hardware-configuration.nix
    inputs.self.nixosProfiles.desktop
    inputs.self.nixosModules.print-scan
  ];
  deviceSpecific.devInfo = {
    cpu = {
      vendor = "amd";
      clock = 4200;
      cores = 8;
    };
    drive = {
      type = "ssd";
      speed = 6000;
      size = 250;
    };
    bigScreen = true;
    ram = 32;
  };
  deviceSpecific.isHost = true;
  services.apcupsd.enable = true;
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];


  secrets.wireguard-wg0 = { };

  # networking.wireguard.interfaces.serokell = {
  #   listenPort = 51820;
  #   ips = [
  #     "172.20.0.52/32"
  #     # "fd73:7272:ed50::52/128"
  #   ];
  #   privateKeyFile = config.secrets.wireguard-serokell.decrypted;
  #   peers = [{
  #     allowedIPs = [
  #       "0.0.0.0/0"
  #       # "::/0"
  #     ];
  #     # endpoint = "serokell.net:35944";
  #     endpoint = "147.75.100.17:35944";
  #     publicKey = "sgLUARawWJejANs2CwuCptwJO55c4jkmnP0L14FNCyw=";
  #     persistentKeepalive = 24;
  #   }];
  # };

  services.ezwg = {
    enable = true;
    proxy = true;
    lanSize = 32;
    serverIP = "147.75.100.17";
    serverPort = 35944;
    serverKey = "sgLUARawWJejANs2CwuCptwJO55c4jkmnP0L14FNCyw=";
    privateKeyFile = config.secrets.wireguard-wg0.decrypted;
    vlanIP = "172.20.0.52";
  };

  # restart when the service fails to resolve DNS
  systemd.services.wireguard-serokell.serviceConfig = {
    Restart = "on-failure";
    RestartSec = "30s";
  };
}
