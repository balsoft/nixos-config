{
  services.unbound = {
    enable = true;
    settings = {
      server = {
        interface = [ "127.0.0.1" ];
      };
    };
  };

  networking.nameservers = [
    "127.0.0.1"
    "1.1.1.1"
  ];
  networking.resolvconf.useLocalResolver = true;
}
