{ config, lib, pkgs, ... }: {

  services.acpid.enable = true;
  programs.ssh.startAgent = true;

  power.ups.enable = config.device == "AMD-Workstation";
  

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

  services.avahi.enable = true;

  systemd.services.systemd-udev-settle.enable = false;

  services.nix-serve.enable = config.device == "AMD-Workstation";
  
  services.upower.enable = true;
  virtualisation.docker.enable = config.deviceSpecific.isHost;
  virtualisation.libvirtd.enable = config.deviceSpecific.isHost;
}
