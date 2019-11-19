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
      text = config.secrets.ssl.cert;
    });
    tls_private_key_path = toString (pkgs.writeTextFile {
      name = "matrix_rsa";
      text = config.secrets.ssl.priv;
    });
    app_service_config_files = [
      (builtins.toFile "registration_tg.yaml"
        (builtins.toJSON config.secrets.matrix.mautrix-telegram.registration))
      (builtins.toFile "registration_wa.yaml"
        (builtins.toJSON config.secrets.matrix.mautrix-whatsapp.registration))
    ];
  };
  systemd.services.whatsapp-vm =    lib.mkIf (config.device == "AMD-Workstation")
  {
    description = "Android VM with WhatsApp";
    wantedBy = [ "multi-user.target" ];
    requires = [ "network-online.target" ];
    preStart = "mkdir -p /var/lib/whatsapp";
    serviceConfig.Type = "forking";
    script = ''
      NIX_ANDROID_EMULATOR_FLAGS="-no-audio -no-window" ${
        with import <nixpkgs> {
          config.android_sdk.accept_license = true;
        };
        androidenv.emulateApp {
          name = "WhatsApp";
          app = fetchurl {
            url =
              "https://www.cdn.whatsapp.net/android/2.19.214/WhatsApp.apk";
              sha256 =
                "1yc8zhlx86gb2ixizxgm2mp6dz8c47xa7w7jjaisb2v4ywmlmdmh";
          };
          platformVersion = "18";
          abiVersion = "x86";
          
          package = "com.whatsapp";
          activity = ".HomeActivity";
          
          avdHomeDir = "/var/lib/whatsapp";
        }
      }/bin/run-test-emulator
    '';
  };
  systemd.services.mautrix-whatsapp =
    lib.mkIf (config.device == "AMD-Workstation") {
      description = "A bridge between whatsapp and matrix";
      path = with pkgs; [ coreutils mautrix-whatsapp ];
      wantedBy = [ "multi-user.target" ];
      requires = [ "matrix-synapse.service" "whatsapp-vm.service" "network-online.target" ];
      serviceConfig = {
        Restart = "always";
        RestartSec = 1;
      };
      script = ''
        mkdir -p /var/lib/mautrix-whatsapp
        cd /var/lib/mautrix-whatsapp
        sleep 5
        timeout 900 mautrix-whatsapp -c ${
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
      serviceConfig = {
        Restart = "always";
        RestartSec = 1;
      };
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
        timeout 900 mautrix-telegram 
      '';
    };
}
