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
    path = [pkgs.gcalcli pkgs.python3];
    script = "python3 ${./mai2google.py}";
  };
  systemd.timers.mai2google = lib.mkIf (config.device == "AMD-Workstation") {
    timerConfig.OnCalendar = "00:00:00";
  };
}
