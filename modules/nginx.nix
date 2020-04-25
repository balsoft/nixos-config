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
        locations."/" = {
          root = "/var/lib/balsoft.ru";
          index = "index.txt";
        };
        enableACME = true;
        addSSL = true;
      };
      "code.balsoft.ru" = {
        locations."/" = { proxyPass = "http://localhost:6000"; };
      } // default;
      "cache.balsoft.ru" = {
        locations."/" = { proxyPass = "http://localhost:5000"; };
      } // default;
      "matrix.balsoft.ru" = {
        locations."/" = { proxyPass = "https://localhost:13748"; };
      } // default;
      "share.balsoft.ru" = {
        locations."/" = { root = "/var/lib/share"; };
      } // default;
    };
  };
  security.acme = {
    email = "balsoft@balsoft.ru";
    acceptTerms = true;
  };
}
