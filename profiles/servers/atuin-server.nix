{ config, pkgs, ... }: {
  services.atuin = {
    enable = true;
    database = {
      uri = "sqlite:///var/lib/atuin/db.sqlite";
      createLocally = false;
    };
  };
  systemd.services.atuin.serviceConfig.StateDirectory = "atuin";
  services.nginx.virtualHosts."atuin.balsoft.eu" = {
    enableACME = true;
    forceSSL = true;
    locations = {
      "/".proxyPass = "http://127.0.0.1:8888";
    };
  };
}
