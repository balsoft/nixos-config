{ pkgs, config, lib, ... }: {
  services.nginx = lib.mkIf (config.device == "AMD-Workstation") {
    enable = true;
    virtualHosts = let
      default = {
        addSSL = true;
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
        locations."/" = { proxyPass = "http://localhost:1337"; };
      } // default;
    };
  };
  systemd.services.mai = {
    script = "${
        pkgs.python3.withPackages
        (ps: with ps; [ beautifulsoup4 flask ruamel_yaml ])
      }/bin/python3 ${./maiparser.py}";
    wantedBy = [ "multi-user.target" ];
  };
}
