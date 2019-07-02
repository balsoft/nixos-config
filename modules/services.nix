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

  environment.etc."ppp/peers/birevia" = {
    enable = config.device == "AMD-Workstation";
    text = ''
      remotename PPTP
      require-mschap-v2
      require-mppe
      persist
      nodefaultroute
      lcp-echo-failure 4
      lcp-echo-interval 30
      maxfail 0
      mtu 1450
      mru 1450
      pty "pptp 185.195.25.201 --nolaunchpppd"
      name ${config.secrets.birevia.user}
      password ${config.secrets.birevia.password}
      linkname birevia
      lock
      noauth
      refuse-pap
      refuse-eap
      refuse-chap
      refuse-mschap
      nobsdcomp
      nodeflate
    '';
  };

  systemd.services.birevia = {
    enable = config.device == "AMD-Workstation";
    path = with pkgs; [ ppp pptp ];
    wantedBy = [ "multi-user.target" ];
    script = ''
      ping 172.17.1.1 -c 1 > /dev/null
      if [ "$?" = 0 ]
        then
        exit 0
      fi
      poff birevia
      pppd call birevia updetach
      route add -net 172.17.1.0 netmask 255.255.255.0 gw 185.195.25.201
    '';
    serviceConfig = {
      User = "root";
      Restart = "always";
      RestartSec = "300";
    };
  };

  services.upower.enable = true;
  virtualisation.docker.enable = config.deviceSpecific.isHost;
  virtualisation.virtualbox.host = {
    enable = config.deviceSpecific.isHost;
    enableHardening = false;
    enableExtensionPack = true;
  };

}
