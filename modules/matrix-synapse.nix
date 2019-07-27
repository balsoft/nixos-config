{ pkgs, config, ... }: {
  services.matrix-synapse = {
    enable = true;
    allow_guest_access = true;
    listeners = [{
      bind_address = "0.0.0.0";
      port = 13748;
      resources = [
        {
          compress = true;
          names = [ "client" ];
        }
        {
          compress = false;
          names = [ "federation" ];
        }
      ];
      tls = false;
      type = "http";
      x_forwarded = false;
    }];
    registration_shared_secret =
    config.secrets.matrix.shared_secret;
    public_baseurl = "http://balsoft.online/13748";
  };
}
