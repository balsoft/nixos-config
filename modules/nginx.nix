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
  systemd.services.mai = lib.mkIf (config.device == "AMD-Workstation") {
    script = "${
        pkgs.python3.withPackages
        (ps: with ps; [ beautifulsoup4 flask ruamel_yaml html5lib ])
      }/bin/python3 ${./maiparser.py}";
    wantedBy = [ "multi-user.target" ];
  };
  systemd.services.mai2google = lib.mkIf (config.device == "AMD-Workstation") {
    path = with pkgs; [ bash gcalcli python3 curl ];
    serviceConfig.User = "balsoft";
    script =
      "curl http://mai.balsoft.ru/json/%D0%9C8%D0%9E-106%D0%91-19 | python3 ${
        ./mai2google.py
      }";
  };
  systemd.timers.mai2google = lib.mkIf (config.device == "AMD-Workstation") {
    timerConfig.OnUnitActiveSec = "3600";
  };
}
