{ pkgs, lib, config, ... }: {
  networking = {
    networkmanager.enable = true;
    firewall.enable = false;
    usePredictableInterfaceNames = false;
    hostName = config.device;
  };
  systemd.services.dhcpcd.serviceConfig.Type = lib.mkForce
    "simple"; # TODO Make a PR with this change; forking is not acceptable for dhcpcd.
}
