{ config, pkgs, ... }: {
  services.nextcloud = {
    enable = true;
    hostName = "nextcloud.balsoft.ru";
    config.adminpassFile = config.secrets.nextcloud.decrypted;
    package = pkgs.nextcloud27;
    enableBrokenCiphersForSSE = false;
    https = true;
  };
  secrets.nextcloud = {
    owner = "nextcloud:nextcloud";
    services = [ "nextcloud-setup" ];
  };
  services.nginx.virtualHosts."nextcloud.balsoft.ru" = {
    enableACME = true;
    forceSSL = true;
    locations."/".proxyWebsockets = true;
  };
}
