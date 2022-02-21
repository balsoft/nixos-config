{ config, pkgs, lib, ... }:
{
  services.nextcloud = {
    enable = true;
    hostName = "nextcloud.balsoft.ru";
    config.adminpassFile = config.secrets.nextcloud.decrypted;
    package = pkgs.nextcloud23;
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
    locations."~ ^\\/(?:build|tests|config|lib|3rdparty|templates|data)\\/".proxyWebsockets = true;
    locations."~ ^\\/(?:index|remote|public|cron|core/ajax\\/update|status|ocs\\/v[12]|updater\\/.+|ocs-provider\\/.+|ocm-provider\\/.+)\\.php(?:$|\\/)".proxyWebsockets = true;
  };
}
