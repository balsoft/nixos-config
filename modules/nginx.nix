{ pkgs, config, lib, ... }: {
  services.nginx = lib.mkIf (config.device == "AMD-Workstation") {
    enable = true;
    appendHttpConfig = "charset utf-8;";
    virtualHosts = let
      default = {
        forceSSL = true;
        enableACME = true;
      };
    in {
      "cache.balsoft.ru" = {
        locations."/" = { proxyPass = "http://localhost:5000"; };
      } // default;
      "matrix.balsoft.ru" = {
        locations."/" = { proxyPass = "https://localhost:13748"; };
      } // default;
      "mai.balsoft.ru" = {
        locations."/" = { root = "/var/lib/important"; };
        locations."/api" = { proxyPass = "http://localhost:1337"; };
      } // default;
    };
  };
}
