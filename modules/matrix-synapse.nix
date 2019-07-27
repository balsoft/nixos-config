{ pkgs, config, ... }: {
  services.matrix-synapse = {
    enable = true;
    allow_guest_access = true;
    listeners = [{
      bind_address = "balsoft.online";
      port = 13748;
      resources = [
        {
          compress = true;
          names = [ "client" "webclient" ];
        }
        {
          compress = false;
          names = [ "federation" ];
        }
      ];
      tls = true;
      type = "http";
      x_forwarded = false;
    }];
    registration_shared_secret =
    config.secrets.matrix.shared_secret;
    public_baseurl = "http://balsoft.online/13748";
  };
}
