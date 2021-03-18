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
        default = true;
        locations."/" = {
          root = "/var/lib/balsoft.ru";
          index = "index.txt";
        };
        locations."/.well-known" = {
          proxyPass = "http://localhost:13748";
        };
        locations."/_matrix" = {
          proxyPass = "http://localhost:13748";
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
        locations."/" = {
          proxyPass = "http://localhost:13748";
        };
      } // default;
      "share.balsoft.ru" = {
        locations."/" = { root = "/var/lib/share"; };
      } // default;
      "things.balsoft.ru" = {
        locations."/" = { root = "/nix/var/nix/profiles/per-user/nginx/random-things/www"; };
      } // default;
    };
  };
  security.acme = {
    email = "balsoft@balsoft.ru";
    acceptTerms = true;
  };
}
