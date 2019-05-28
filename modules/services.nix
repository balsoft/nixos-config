{ config, lib, pkgs, ... }: {

  services.acpid.enable = true;

  services.mopidy = {
    enable = true;
    extensionPackages = with pkgs; [mopidy-gmusic];
    configuration = if (!isNull config.secrets.gpmusic) then ''
      [gmusic]
      username = ${config.secrets.gpmusic.user}
      password = ${config.secrets.gpmusic.password}
      deviceid = ${config.secrets.gpmusic.deviceid}
    '' else
      "";
  };
  
  systemd.services.mopidy.serviceConfig.User = lib.mkForce "balsoft";

  services.earlyoom = {
    enable = true;
    freeMemThreshold = 5;
    freeSwapThreshold = 100;
  };

  services.printing = {
    enable = true;
    drivers = [pkgs.gutenprint];
  };
  programs.dconf.enable = true;

  services.tor = {
    enable = true;
    client.enable = true;
    client.privoxy.enable = true;
    torsocks.enable = true;
    client.socksListenAddressFaster = "0.0.0.0:9063";
  };
  #services.teamviewer.enable = true;

  programs.mosh.enable = true;

  systemd.units."dbus.service".text = lib.mkForce "blah";

  services.accounts-daemon.enable = true;
  services.avahi.enable = true;

  systemd.services.systemd-udev-settle.enable = false;

  services.upower.enable = true;
  virtualisation.docker.enable = config.deviceSpecific.isHost;
  virtualisation.virtualbox.host = {
    enable = config.deviceSpecific.isHost;
    enableHardening = false;
  };

}
