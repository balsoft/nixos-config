{ pkgs, config, lib, ... }: {
  services.matrix-synapse = {
    enable = true;
    allow_guest_access = false;
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
      type = "http";
      tls = false;
      x_forwarded = true;
    }];
    public_baseurl = "https://balsoft.ru";
    server_name = "balsoft.ru";
    app_service_config_files =
      [ config.secrets-envsubst.mautrix-telegram-registration.substituted ];
  };

  services.postgresql.enable = true;

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
        bot_messages_as_notices = true;
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
      };
      homeserver = {
        address = "https://matrix.balsoft.ru";
        domain = "balsoft.ru";
        verify_ssl = true;
      };
      telegram = { bot_token = "disabled"; };
    };
  };

  secrets-envsubst.mautrix-telegram = {
    secrets = [ "as_token" "hs_token" "api_id" "api_hash" ];
    template = ''
      MAUTRIX_TELEGRAM_APPSERVICE_AS_TOKEN=$as_token
      MAUTRIX_TELEGRAM_APPSERVICE_HS_TOKEN=$hs_token
      MAUTRIX_TELEGRAM_TELEGRAM_API_ID=$api_id
      MAUTRIX_TELEGRAM_TELEGRAM_API_HASH=$api_hash
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

  systemd.services.mautrix-telegram.serviceConfig.DynamicUser =
    lib.mkForce false;

  systemd.services.mautrix-telegram.serviceConfig.User = "mautrix-telegram";

  users.users.mautrix-telegram = {
    group = "mautrix-telegram";
    isSystemUser = true;
  };

  users.groups.mautrix-telegram = { };

  users.users.matrix-synapse.name = lib.mkForce "matrix-synapse";
}
