{ config, lib, pkgs, ... }: {

  services.acpid.enable = true;

  services.apcupsd = { enable = config.device == "AMD-Workstation"; };

  services.earlyoom = {
    enable = config.devices.${config.device}.ram < 16;
    freeMemThreshold = 5;
    freeSwapThreshold = 100;
  };

  services.printing = {
    enable = true;
    drivers = [ pkgs.gutenprint ];
  };
  programs.dconf.enable = true;

  services.tor = {
    enable = true;
    client.enable = true;
    client.privoxy.enable = true;
    torsocks.enable = true;
    client.socksListenAddressFaster = "127.0.0.1:9063";
  };

  programs.mosh.enable = true;

  services.fwupd.enable = true;

  services.avahi.enable = true;

  systemd.services.systemd-udev-settle.enable = false;

  services.nix-serve.enable = config.device == "AMD-Workstation";

  services.pcscd.enable = true;

  services.keybase.enable = true;
  
  services.upower.enable = true;
  virtualisation.docker.enable = config.deviceSpecific.isHost;
  virtualisation.libvirtd = {
    enable = config.deviceSpecific.isHost;
  };

  services.vsftpd = {
    enable = true;
    anonymousUser = true;
  };
  networking.firewall.trustedInterfaces = [ "eth0" ];

  services.nextcloud = {
    enable = true;
    nginx.enable = true;
    hostName = "nextcloud.balsoft.ru";
    config.adminpassFile = "/home/balsoft/nextcloud-admin";
    package = pkgs.nextcloud19;
  };

  services.nginx.virtualHosts."nextcloud.balsoft.ru" = {
    enableACME = true;
    forceSSL = true;
  };

}
