{ config, pkgs, lib, inputs, ... }: {
  services.home-assistant = {
    enable = true;
    package = (pkgs.home-assistant.override {
      extraPackages = py: with py; [ aiohttp-cors zeroconf pycrypto ];
    }).overrideAttrs (_: {
      tests = [];
      doInstallCheck = false;
    });
    config = {
      homeassistant = {
        name = "Home";
        time_zone = "Europe/Moscow";
        latitude = 1;
        longitude = 1;
        elevation = 1;
        unit_system = "metric";
        temperature_unit = "C";
      };
      # Enable the frontend
      frontend = { };
      sonoff = {
        default_class = "light";
      };
      mobile_app = { };
    };
  };
  systemd.tmpfiles.rules = [
    "C /var/lib/hass/custom_components/sonoff - - - - ${inputs.sonoff-lan}/custom_components/sonoff"
    "Z /var/lib/hass 770 hass hass - -"
  ];
  services.nginx = {
    virtualHosts."hass.balsoft.ru" = {
      enableACME = true;
      forceSSL = true;
      extraConfig = ''
        proxy_buffering off;
      '';
      locations."/" = {
        proxyPass = "http://127.0.0.1:8123";
        proxyWebsockets = true;
        extraConfig = ''
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header Upgrade $http_upgrade;
          proxy_set_header Connection $connection_upgrade;
        '';
      };
    };
  };
}
