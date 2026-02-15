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
        "balsoft.ru" = {
          default = true;
          locations."/" = {
            root = "/var/lib/balsoft.ru";
            index = "index.txt";
          };
          locations."/.well-known/matrix" = {
            root = pkgs.writeTextFile {
              name = "well-known-matrix";
              destination = "/.well-known/matrix/client";
              text = builtins.toJSON {
                "m.homeserver" = {
                  base_url = "https://balsoft.ru";
                };
                "m.identity_server" = {
                  base_url = "https://vector.im";
                };
                "org.matrix.msc3575.proxy" = {
                  url = "https://balsoft.ru";
                };
                "org.matrix.msc4143.rtc_foci" = [
                  {
                    livekit_service_url = "https://balsoft.ru";
                    type = "livekit";
                  }
                ];
              };
            };
            extraConfig = ''
              default_type application/json;
            '';
          };
          locations."/_matrix" = {
            proxyPass = "http://localhost:13748";
          };
          enableACME = true;
          forceSSL = true;
        };
        "code.balsoft.ru" = {
          locations."/" = {
            proxyPass = "http://localhost:6000";
          };
        }
        // default;
        "cache.balsoft.ru" = {
          locations."/" = {
            proxyPass = "http://localhost:5000";
          };
        }
        // default;
        "matrix.balsoft.ru" = {
          locations."/" = {
            proxyPass = "http://localhost:13748";
          };
        }
        // default;
        "share.balsoft.ru" = {
          locations."/" = {
            root = "/var/lib/share";
          };
        }
        // default;
        "things.balsoft.ru" = {
          locations."/" = {
            root = "/nix/var/nix/profiles/per-user/nginx/random-things/www";
          };
        }
        // default;
      };
  };
  security.acme.defaults.email = "balsoft@balsoft.ru";
  security.acme.acceptTerms = true;
}
