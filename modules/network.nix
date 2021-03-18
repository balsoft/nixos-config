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

}
