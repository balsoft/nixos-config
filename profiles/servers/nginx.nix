{
  pkgs,
  config,
  lib,
  ...
}:
{
  services.nginx = {
    enable = true;
    appendHttpConfig = "charset utf-8;";
    virtualHosts =
      let
        default = {
          forceSSL = true;
          enableACME = true;
        };
      in
      {
        "balsoft.eu" = {
          default = true;
          quic = true;
          http3 = true;
          locations."/" = {
            root = "/var/lib/balsoft.eu";
            index = "index.txt";
          };
          locations."/.well-known/matrix" = {
            # proxyPass = "http://localhost:13748";
            proxyPass = "http://unix:/run/continuwuity/continuwuity.sock";
          };
          locations."/_matrix" = {
            # proxyPass = "http://localhost:13748";
            proxyPass = "http://unix:/run/continuwuity/continuwuity.sock";
          };
          locations."/_conduwuit" = {
            # proxyPass = "http://localhost:13748";
            proxyPass = "http://unix:/run/continuwuity/continuwuity.sock";
          };
          locations."/_continuwuity" = {
            # proxyPass = "http://localhost:13748";
            proxyPass = "http://unix:/run/continuwuity/continuwuity.sock";
          };
          enableACME = true;
          forceSSL = true;
        };
        "matrix.balsoft.eu" = {
          locations."/" = {
            # proxyPass = "http://localhost:13748";
            proxyPass = "http://unix:/run/continuwuity/continuwuity.sock";
          };
        }
        // default;
        "share.balsoft.eu" = {
          locations."/" = {
            root = "/var/lib/share";
          };
        }
        // default;
        "things.balsoft.eu" = {
          locations."/" = {
            root = "/nix/var/nix/profiles/per-user/nginx/random-things/www";
          };
        }
        // default;
      };
  };
  security.acme.defaults.email = "balsoft@balsoft.eu";
  security.acme.acceptTerms = true;
}
