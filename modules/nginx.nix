{ pkgs, config, lib, ... }: {
  services.nginx = {
    enable = true;
    appendHttpConfig = "charset utf-8;";
    virtualHosts = let
      default = {
        forceSSL = true;
        enableACME = true;
      };
    in {
      "balsoft.ru" = {
        locations."/" = { root = "/var/lib/balsoft.ru"; index = "index.txt"; };
        enableACME = true;
        addSSL = true;
      };
      "cache.balsoft.ru" = {
        locations."/" = { proxyPass = "http://localhost:5000"; };
      } // default;
      "matrix.balsoft.ru" = {
        locations."/" = { proxyPass = "https://localhost:13748"; };
      } // default;
      "mai.balsoft.ru" = {
        locations."/" = { root = "/var/lib/important"; };
      } // default;
      "share.balsoft.ru" = {
        locations."/" = { root = "/var/lib/share"; };
      } // default;
    };
  };
}
