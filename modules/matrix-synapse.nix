{ pkgs, config, lib, ... }: {
  services.matrix-synapse = lib.mkIf (config.device == "AMD-Workstation") {
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
      type = "http";
      x_forwarded = false;
    }];
    registration_shared_secret = config.secrets.matrix.shared_secret;
    public_baseurl = "https://balsoft.ru:13748/";
    server_name = "balsoft.ru";
    tls_certificate_path = toString (pkgs.writeTextFile {
      name = "matrix.crt";
      text = config.secrets.matrix.cert;
    });
    tls_private_key_path = toString (pkgs.writeTextFile {
      name = "matrix_rsa";
      text = config.secrets.matrix.priv;
    });
    app_service_config_files = [
      (builtins.toFile "registration_tg.yaml"
      (builtins.toJSON config.secrets.matrix.mautrix-telegram.registration))
      (builtins.toFile "registration_wa.yaml"
      (builtins.toJSON config.secrets.matrix.mautrix-whatsapp.registration))
    ];
  };
  systemd.services.mautrix-whatsapp =
    lib.mkIf (config.device == "AMD-Workstation") {
      description = "A bridge between whatsapp and matrix";
      path = with pkgs; [ coreutils mautrix-whatsapp ];
      wantedBy = [ "network-online.target" ];
      requires = [ "matrix-synapse.service" ];
      script = ''
        mkdir -p /var/lib/mautrix-whatsapp
        cd /var/lib/mautrix-whatsapp
        sleep 5
        mautrix-whatsapp -c ${
          builtins.toFile "config_wa.yaml"
          (builtins.toJSON config.secrets.matrix.mautrix-whatsapp.config)
        }
      '';
    };
  systemd.services.mautrix-telegram =
    lib.mkIf (config.device == "AMD-Workstation") {
      description = "A bridge between telegram and matrix";
      requires = [ "matrix-synapse.service" "openvpn-client.service" ];
      path = with pkgs; [
        coreutils
        mautrix-telegram
        (python3.pkgs.alembic.overrideAttrs (old: {
          propagatedBuildInputs = old.propagatedBuildInputs
            ++ [ mautrix-telegram ];
        }))
      ];
      wantedBy = [ "network-online.target" ];
      script = ''
        mkdir -p /var/lib/mautrix-telegram
        cp -r ${pkgs.mautrix-telegram}/* /var/lib/mautrix-telegram
        cd /var/lib/mautrix-telegram
        alembic upgrade head || echo "update failed"
        sleep 5
        cp ${
          builtins.toFile "config.yaml"
          (builtins.toJSON config.secrets.matrix.mautrix-telegram.config)
        } ./config.yaml
        mautrix-telegram
      '';
    };
}