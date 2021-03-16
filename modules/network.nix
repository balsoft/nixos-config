{ pkgs, lib, config, ... }:
let
  localRanges = [{
    from = 1714;
    to = 1764;
  } # KDE connect
    ];
in {
  networking = {
    networkmanager.enable = true;
    firewall = {
      enable = true;
      allowedTCPPorts = [ 13748 13722 5000 22 80 443 51820 ];
      interfaces.wlan0.allowedTCPPortRanges = localRanges;
      interfaces.wlan0.allowedUDPPortRanges = localRanges;
      interfaces.eth0.allowedUDPPortRanges = localRanges;
      interfaces.eth0.allowedTCPPortRanges = localRanges;
    };
    resolvconf.extraConfig = ''
      local_nameservers=""
      name_server_blacklist="0.0.0.0 127.0.0.1"
      resolv_conf_local_only=NO
    '';
    usePredictableInterfaceNames = false;
    hostName = config.device;
  };
  networking.firewall.trustedInterfaces = [ "eth0" ];
  systemd.services.ModemManager.wantedBy =
    lib.optional (config.device == "T490s-Laptop") "network.target";

  secrets.wireguard-serokell = { };

  networking.wireguard.interfaces.serokell = {
    listenPort = 51820;
    ips = [
      "172.20.0.52/32"
      # "fd73:7272:ed50::52/128"
    ];
    privateKeyFile = config.secrets.wireguard-serokell.decrypted;
    peers = [{
      allowedIPs = [
        "0.0.0.0/0"
        # "::/0"
      ];
      # endpoint = "serokell.net:35944";
      endpoint = "147.75.100.17:35944";
      publicKey = "sgLUARawWJejANs2CwuCptwJO55c4jkmnP0L14FNCyw=";
      persistentKeepalive = 24;
    }];
  };

  # restart when the service fails to resolve DNS
  systemd.services.wireguard-serokell.serviceConfig = {
    Restart = "on-failure";
    RestartSec = "30s";
  };
}
