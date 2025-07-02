{ pkgs, config, lib, ... }: {
  services.matrix-synapse = {
    enable = true;
    settings = {
      allow_guest_access = false;
      listeners = [{
        # bind_address = "0.0.0.0";
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
        type = "http";
        tls = false;
        x_forwarded = true;
      }];
      public_baseurl = "https://balsoft.ru";
      server_name = "balsoft.ru";
      turn_uris =
        [ "turn:balsoft.ru?transport=udp" "turn:balsoft.ru?transport=tcp" ];
      # app_service_config_files =
      #   [ config.secrets-envsubst.mautrix-telegram-registration.substituted ];
      allow_public_rooms_over_federation = true;
      media_retention.remote_media_lifetime = "14d";
    };
    extraConfigFiles = [
      config.secrets-envsubst.coturn.substituted
      config.secrets-envsubst.matrix.substituted
    ];
  };

  services.postgresql.enable = true;
  services.postgresql.package = pkgs.postgresql_15;

  services.mautrix-telegram = {
    enable = true;
    environmentFile = toString config.secrets-envsubst.mautrix-telegram;
    settings = {
      appservice = {
        address = "http://localhost:29317";
        bot_avatar = "mxc://maunium.net/tJCRmUyJDsgRNgqhOgoiHWbX";
        id = "telegram";
        max_body_size = 1;
        port = 29317;
      };
      bridge = {
        alias_template = "tg_{groupname}";
        allow_matrix_login = true;
        bot_messages_as_notices = false;
        catch_up = true;
        command_prefix = "!tg";
        image_as_file_size = 10;
        max_document_size = 100;
        max_initial_member_sync = -1;
        max_telegram_delete = 10;
        permissions = {
          "*" = "relaybot";
          "@balsoft:balsoft.ru" = "admin";
        };
        plaintext_highlights = true;
        startup_sync = true;
        username_template = "tg_{userid}";
        relay_user_distinguishers = [ ];
        state_event_formats = {
          leave = "";
          name_change = "";
        };
      };
      homeserver = {
        address = "https://matrix.balsoft.ru";
        domain = "balsoft.ru";
        verify_ssl = true;
      };
    };
  };

  secrets-envsubst.mautrix-telegram = {
    secrets = [ "as_token" "hs_token" "api_id" "api_hash" "bot_token" ];
    template = ''
      MAUTRIX_TELEGRAM_APPSERVICE_AS_TOKEN=$as_token
      MAUTRIX_TELEGRAM_APPSERVICE_HS_TOKEN=$hs_token
      MAUTRIX_TELEGRAM_TELEGRAM_API_ID=$api_id
      MAUTRIX_TELEGRAM_TELEGRAM_API_HASH=$api_hash
      MAUTRIX_TELEGRAM_TELEGRAM_BOT_TOKEN=$bot_token
    '';
  };

  secrets-envsubst.mautrix-telegram-registration = {
    directory = "mautrix-telegram";
    secrets = [ "as_token" "hs_token" ];
    owner = "matrix-synapse";
    template = builtins.toJSON {
      as_token = "$as_token";
      hs_token = "$hs_token";
      id = "telegram";
      namespaces = {
        aliases = [{
          exclusive = true;
          regex = "#tg_.+:balsoft.ru";
        }];
        users = [{
          exclusive = true;
          regex = "@tg_.+:balsoft.ru";
        }];
      };
      rate_limited = false;
      sender_localpart = "telegrambot";
      url = "http://localhost:29317";
    };
  };

  systemd.services.mautrix-telegram.serviceConfig = {
    DynamicUser = lib.mkForce false;
    Restart = "always";
    RuntimeMaxSec = "1d";
  };

  systemd.services.mautrix-telegram.serviceConfig.User = "mautrix-telegram";

  users.users.mautrix-telegram = {
    group = "mautrix-telegram";
    isSystemUser = true;
  };

  users.groups.mautrix-telegram = { };

  users.users.matrix-synapse.name = lib.mkForce "matrix-synapse";

  services.coturn = {
    enable = true;
    use-auth-secret = true;
    static-auth-secret-file = config.secrets.coturn.decrypted;
    no-tls = true;
    realm = "balsoft.ru";
    extraConfig = ''
      external-ip=94.25.150.197

      denied-peer-ip=10.0.0.0-10.255.255.255
      denied-peer-ip=192.168.0.0-192.168.255.255
      denied-peer-ip=172.16.0.0-172.31.255.255

      denied-peer-ip=0.0.0.0-0.255.255.255
      denied-peer-ip=100.64.0.0-100.127.255.255
      denied-peer-ip=127.0.0.0-127.255.255.255
      denied-peer-ip=169.254.0.0-169.254.255.255
      denied-peer-ip=192.0.0.0-192.0.0.255
      denied-peer-ip=192.0.2.0-192.0.2.255
      denied-peer-ip=192.88.99.0-192.88.99.255
      denied-peer-ip=198.18.0.0-198.19.255.255
      denied-peer-ip=198.51.100.0-198.51.100.255
      denied-peer-ip=203.0.113.0-203.0.113.255
      denied-peer-ip=240.0.0.0-255.255.255.255

      allowed-peer-ip=192.168.8.236
    '';
  };

  secrets.coturn = {
    encrypted =
      "/home/balsoft/.local/share/password-store/coturn/shared_secret.gpg";
    services = [ "coturn" ];
    owner = "turnserver:turnserver";
  };
  secrets-envsubst.coturn = {
    secrets = [ "shared_secret" ];
    services = [ "matrix-synapse" ];
    owner = "matrix-synapse:matrix-synapse";
    template = builtins.toJSON { turn_shared_secret = "$shared_secret"; };
  };
  secrets-envsubst.matrix = {
    secrets = [ "registration_shared_secret" ];
    services = [ "matrix-synapse" ];
    owner = "matrix-synapse:matrix-synapse";
    template = builtins.toJSON {
      registration_shared_secret = "$registration_shared_secret";
    };
  };

  networking.firewall = rec {
    allowedTCPPorts = [ 3478 5349 ];
    allowedUDPPorts = allowedTCPPorts;
    allowedUDPPortRanges = [{
      from = 49152;
      to = 65535;
    }];
  };

}
