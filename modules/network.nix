{ pkgs, lib, config, ... }: {
  networking = {
    networkmanager.enable = true;
    firewall.enable = false;
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
  # systemd.services.dhcpcd.serviceConfig.Type = lib.mkForce
    # "simple"; # TODO Make a PR with this change; forking is not acceptable for dhcpcd.
}
