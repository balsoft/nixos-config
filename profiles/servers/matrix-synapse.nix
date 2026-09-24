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
  services.matrix-continuwuity = {
    enable = true;
    settings = {
      global = {
        server_name = "balsoft.eu";
        # port = [ 13748 ];
        cache_capacity_modifier = 2.0;
        unix_socket_path = "/run/continuwuity/continuwuity.sock";
        admins_list = [ "@balsoft:balsoft.eu" ];
        well_known = {
          client = "https://balsoft.eu";
          server = "balsoft.eu:443";
          support_page = "https://balsoft.eu";
          support_role = "m.role.admin";
          support_email = "balsoft@balsoft.eu";
        };
        matrix_rtc.foci = [
          {
            type = "livekit";
            livekit_service_url = "https://livekit.balsoft.eu";
          }
        ];
      };
    };
  };

  users.users.nginx.extraGroups = [ "continuwuity" ];

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
          "@balsoft:balsoft.eu" = "admin";
          "@lyona:balsoft.eu" = "full";
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
        address = "https://balsoft.eu";
        domain = "balsoft.eu";
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
    owner = "matrix-continuwuity";
    template = builtins.toJSON {
      as_token = "$as_token";
      hs_token = "$hs_token";
      id = "telegram";
      namespaces = {
        aliases = [
          {
            exclusive = true;
            regex = "#tg_.+:balsoft.eu";
          }
        ];
        users = [
          {
            exclusive = true;
            regex = "@tg_.+:balsoft.eu";
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

  services.coturn = {
    enable = true;
    use-auth-secret = true;
    static-auth-secret-file = config.secrets.coturn.decrypted;
    no-tls = true;
    realm = "balsoft.eu";
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
    services = [ "matrix-continuwuity" ];
    owner = "matrix-continuwuity:matrix-continuwuity";
    template = builtins.toJSON { turn_shared_secret = "$shared_secret"; };
  };
  secrets-envsubst.matrix = {
    secrets = [ "registration_shared_secret" ];
    services = [ "matrix-continuwuity" ];
    owner = "matrix-continuwuity:matrix-continuwuity";
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
    livekitUrl = "wss://balsoft.eu/livekit/sfu";
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
  systemd.services.lk-jwt-service.environment.LIVEKIT_FULL_ACCESS_HOMESERVERS = "balsoft.eu";
  services.nginx.virtualHosts."balsoft.eu".locations = {
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
