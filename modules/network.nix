{ pkgs, lib, config, ... }: {
  networking = {
    networkmanager.enable = false;
    wireless = {
      enable = true;
      driver = "wext";
      networks.Keenetic.pskRaw =
        "4d03ac6e3d2a2b891d83dcceca6f531abd0fec421ad4460878f5f3bc4c76562e";
      networks.BantyevIE.pskRaw =
        "e3a09c5136e71fa7022a407ffcd0a75acdb4cb733a123bb179dcb5c4762ffe18";
      networks.MikroTik-8649AA.pskRaw =
        "c21f926c600c2848fa1a65c2b4741d3f1c5f80783e805aa5c24dfdab856a3644";
      networks.YBS.pskRaw =
        "0fa37340cc1a743cde290355f649351cb6cfc05371a270193f70bf2a9354613b";
      networks.zzznet.pskRaw =
        "67285ec534ce99e9db95a84751fb70234377aa8b346f7f2b2252a9cde68e108d";
      interfaces = [ "wlan0" ];
      userControlled.enable = true;
    };
    firewall.enable = false;
    usePredictableInterfaceNames = false;
    hostName = config.device;
  };
  systemd.services.dhcpcd.serviceConfig.Type = lib.mkForce
    "simple"; # TODO Make a PR with this change; forking is not acceptable for dhcpcd.
}
