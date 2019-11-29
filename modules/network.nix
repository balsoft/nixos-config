{ pkgs, lib, config, ... }: {
  networking = {
    networkmanager.enable = true;
    firewall = {
      enable = true;
      allowedTCPPorts = [ 13748 13722 5000 22 80 443 ];
      allowedTCPPortRanges = [{
        from = 1714;
        to = 1764;
      }];
      allowedUDPPortRanges = [{
        from = 1714;
        to = 1764;
      }];
    };
    resolvconf.extraConfig = ''
      local_nameservers=""
      name_server_blacklist="0.0.0.0 127.0.0.1"
      resolv_conf_local_only=NO
    '';
    usePredictableInterfaceNames = false;
    hostName = config.device;
  };
  systemd.services.ModemManager.wantedBy =
    lib.optional (config.device == "ThinkPad-Laptop") "network.target";

}
