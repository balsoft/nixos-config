{ config, pkgs, lib, ... }:
{
  services.nextcloud = {
    enable = true;
    hostName = "nextcloud.balsoft.ru";
    config.adminpassFile = "/home/balsoft/nextcloud-admin";
    package = pkgs.nextcloud19;
    https = true;
  };
  services.nginx.virtualHosts."nextcloud.balsoft.ru" = {
    enableACME = true;
    forceSSL = true;
    locations."/".proxyWebsockets = true;
    locations."~ ^\\/(?:build|tests|config|lib|3rdparty|templates|data)\\/".proxyWebsockets = true;
    locations."~ ^\\/(?:index|remote|public|cron|core/ajax\\/update|status|ocs\\/v[12]|updater\\/.+|ocs-provider\\/.+|ocm-provider\\/.+)\\.php(?:$|\\/)".proxyWebsockets = true;
  };
}
