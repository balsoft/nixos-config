{
  pkgs,
  config,
  lib,
  ...
}:
let
  livekitKeyFile = "/run/livekit.key";
in
{
  services.matrix-synapse = {
    enable = true;
    settings = {
      allow_guest_access = false;
      listeners = [
        {
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
        }
      ];
      public_baseurl = "https://balsoft.ru";
      server_name = "balsoft.ru";
      turn_uris = [
        "turn:balsoft.ru:3478?transport=udp"
        "turn:balsoft.ru:3478?transport=tcp"
      ];
      # app_service_config_files =
      #   [ config.secrets-envsubst.mautrix-telegram-registration.substituted ];
      allow_public_rooms_over_federation = true;
      media_retention.remote_media_lifetime = "14d";

      auto_accept_invites.only_from_local_users = true;

      experimental_features = {
        # MSC3266: Room summary API. Used for knocking over federation
        msc3266_enabled = true;
        # MSC4222: needed for syncv2 state_after. This allows clients to
        # correctly track the state of the room.
        msc4222_enabled = true;
        # MSC4140: Delayed events are required for proper call participation signalling. If disabled it is very likely that you end up with stuck calls in Matrix rooms
        msc4140_enabled = true;
      };

      max_event_delay_duration = "24h";
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
          "@lyona:balsoft.ru" = "full";
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
    secrets = [
      "as_token"
      "hs_token"
      "api_id"
      "api_hash"
      "bot_token"
    ];
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
    secrets = [
      "as_token"
      "hs_token"
    ];
    owner = "matrix-synapse";
    template = builtins.toJSON {
      as_token = "$as_token";
      hs_token = "$hs_token";
      id = "telegram";
      namespaces = {
        aliases = [
          {
            exclusive = true;
            regex = "#tg_.+:balsoft.ru";
          }
        ];
        users = [
          {
            exclusive = true;
            regex = "@tg_.+:balsoft.ru";
          }
        ];
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
      allowed-peer-ip=167.235.153.141
    '';
  };

  secrets.coturn = {
    encrypted = "/home/balsoft/.local/share/password-store/coturn/shared_secret.gpg";
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

  services.livekit = {
    enable = true;
    openFirewall = true;
    settings.room.auto_create = false;
    keyFile = livekitKeyFile;
  };
  services.lk-jwt-service = {
    enable = true;
    # can be on the same virtualHost as synapse
    livekitUrl = "wss://balsoft.ru/livekit/sfu";
    keyFile = livekitKeyFile;
  };
  # generate the key when needed
  systemd.services.livekit-key = {
    before = [
      "lk-jwt-service.service"
      "livekit.service"
    ];
    wantedBy = [ "multi-user.target" ];
    path = with pkgs; [
      livekit
      coreutils
      gawk
    ];
    script = ''
      echo "Key missing, generating key"
      echo "lk-jwt-service: $(livekit-server generate-keys | tail -1 | awk '{print $3}')" > "${livekitKeyFile}"
    '';
    serviceConfig.Type = "oneshot";
    unitConfig.ConditionPathExists = "!${livekitKeyFile}";
  };
  # restrict access to livekit room creation to a homeserver
  systemd.services.lk-jwt-service.environment.LIVEKIT_FULL_ACCESS_HOMESERVERS = "balsoft.ru";
  services.nginx.virtualHosts."balsoft.ru".locations = {
    "^~ /livekit/jwt/" = {
      priority = 400;
      proxyPass = "http://[::1]:${toString config.services.lk-jwt-service.port}/";
    };
    "^~ /livekit/sfu/" = {
      extraConfig = ''
        proxy_send_timeout 120;
        proxy_read_timeout 120;
        proxy_buffering off;

        proxy_set_header Accept-Encoding gzip;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection "upgrade";
      '';
      priority = 400;
      proxyPass = "http://[::1]:${toString config.services.livekit.settings.port}/";
      proxyWebsockets = true;
    };
  };
  networking.firewall = rec {
    allowedTCPPorts = [
      3478
      3479
      5349
    ];
    allowedUDPPorts = allowedTCPPorts;
    allowedUDPPortRanges = [
      {
        from = 49152;
        to = 65535;
      }
    ];
  };

}
