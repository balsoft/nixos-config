{ config, lib, pkgs, ... }: {

  services.acpid.enable = true;

  services.mopidy = {
    enable = true;

    extensionPackages = with pkgs; [ mopidy-gmusic ];
    configuration = (if (!isNull config.secrets.gpmusic) then ''
      [gmusic]
      username = ${config.secrets.gpmusic.user}
      password = ${config.secrets.gpmusic.password}
      deviceid = ${config.secrets.gpmusic.deviceid}
    '' else
      "") + ''
      [mpd]
      enabled = true
      hostname = ::
      port = 6600
      password =
      zeroconf = Mopidy MPD server on ${config.device}
      command_blacklist = listall,listallinfo
    '';
  };

  systemd.services.mopidy.serviceConfig.User = lib.mkForce "balsoft";

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
    hiddenServices.sshThroughNAT.map = [{ port = 22; }];
    torsocks.enable = true;
    client.socksListenAddressFaster = "0.0.0.0:9063";
  };

  programs.mosh.enable = true;

  systemd.units."dbus.service".text = lib.mkForce "blah";

  services.accounts-daemon.enable = true;
  services.avahi.enable = true;

  systemd.services.systemd-udev-settle.enable = false;

  services.nix-serve.enable = true;

  services.upower.enable = true;
  virtualisation.docker.enable = config.deviceSpecific.isHost;
  virtualisation.virtualbox.host = {
    enable = config.deviceSpecific.isHost;
    enableHardening = false;
    enableExtensionPack = true;
  };

}
