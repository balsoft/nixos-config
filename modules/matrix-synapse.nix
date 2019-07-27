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
      tls = true;
      type = "https";
      x_forwarded = false;
    }];
    registration_shared_secret = config.secrets.matrix.shared_secret;
    public_baseurl = "https://balsoft.ru:13748/";
    server_name = "balsoft.ru:13748";
    tls_certificate_path = toString (pkgs.writeTextFile {
      name = "matrix.crt";
      text = config.secrets.matrix.cert;
    });
    tls_private_key_path = toString (pkgs.writeTextFile {
      name = "matrix_rsa";
      text = config.secrets.matrix.priv;
    });
  };
}
