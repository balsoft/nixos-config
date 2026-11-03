{ config, ... }: {
  secrets.radicale = {
    owner = "radicale:radicale";

    services = [ "radicale" ];
  };
  services.radicale = {
    enable = true;
    rights = {
      root = {
        user = ".+";
        collection = "";
        permissions = "R";
      };
      principal = {
        user = ".+";
        collection = "{user}";
        permissions = "RW";
      };
      calendars = {
        user = ".+";
        collection = "{user}/[^/]+";
        permissions = "rw";
      };
    };
    settings = {
      server = {
        hosts = [
          "127.0.0.1:5232"
        ];
      };
      auth = {
        type = "htpasswd";

        htpasswd_filename = config.secrets.radicale.decrypted;
        htpasswd_encryption = "bcrypt";
      };
      storage = {
        filesystem_folder = "/var/lib/radicale/collections";
      };
    };
  };
  services.nginx.virtualHosts."radicale.balsoft.eu" = {
    forceSSL = true;
    enableACME = true;

    locations."/".proxyPass = "http://localhost:5232";
  };
}
