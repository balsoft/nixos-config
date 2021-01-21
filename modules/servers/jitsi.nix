{ config, ... }: {
  services.jitsi-meet = {
    enable = true;
    hostName = "meet.balsoft.ru";
  };

  services.nginx.virtualHosts.${config.services.jitsi-meet.hostName} = {
    enableACME = true;
    forceSSL = true;
    basicAuthFile = "/var/lib/jitsi-auth";
  };
}
