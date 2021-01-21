{ pkgs, config, lib, ... }: {
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
      type = "http";
      tls = false;
      x_forwarded = true;
    }];
    public_baseurl = "https://balsoft.ru";
    server_name = "balsoft.ru";
    app_service_config_files = [
      config.secrets-envsubst.mautrix-telegram-registration.substituted
      config.secrets-envsubst.mautrix-whatsapp-registration.substituted
    ];
  };
  services.postgresql.enable = true;
  systemd.services.mautrix-whatsapp = {
    description = "A bridge between whatsapp and matrix";
    path = with pkgs; [ coreutils mautrix-whatsapp ];
    wantedBy = [ "multi-user.target" ];
    requires = [ "matrix-synapse.service" "network-online.target" ];
    serviceConfig = {
      Restart = "always";
      RestartSec = 1;
      User = "mautrix-whatsapp";
      StateDirectory = "mautrix-whatsapp";
    };
    script = ''
      cd /var/lib/mautrix-whatsapp
      sleep 5
      mautrix-whatsapp -c ${config.secrets-envsubst.mautrix-whatsapp}
    '';
  };
  users.users.mautrix-whatsapp.isSystemUser = true;

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

  secrets-envsubst.mautrix-whatsapp = {
    secrets = [ "as_token" "hs_token" ];
    owner = "mautrix-whatsapp";
    template = builtins.toJSON {
      appservice = {
        address = "http://localhost:29318";
        as_token = "$as_token";
        bot = {
          avatar = "mxc://maunium.net/NeXNQarUbrlYBiPCpprYsRqr";
          displayname = "WhatsApp bridge bot";
          username = "whatsappbot";
        };
        database = {
          max_idle_conns = 2;
          max_open_conns = 20;
          type = "sqlite3";
          uri = "mautrix-whatsapp.db";
        };
        hostname = "0.0.0.0";
        hs_token = "$hs_token";
        id = "whatsapp";
        port = 29318;
        state_store_path = "./mx-state.json";
      };
      bridge = {
        command_prefix = "!wa";
        connection_retry_delay = -1;
        connection_timeout = 20;
        contact_wait_delay = 1;
        displayname_template =
          "{{if .Notify}}{{.Notify}}{{else}}{{.Jid}}{{end}} (WA)";
        initial_chat_sync_count = 10;
        initial_history_fill_count = 20;
        invite_own_puppet_for_backfilling = true;
        max_connection_attempts = 3;
        permissions = {
          "*" = 10;
          "@balsoft:balsoft.ru" = 100;
        };
        private_chat_portal_meta = false;
        recovery_chat_sync_count = -1;
        recovery_history_backfill = true;
        report_connection_retry = true;
        sync_max_chat_age = 259200;
        sync_with_custom_puppets = true;
        username_template = "whatsapp_{{.}}";
      };
      homeserver = {
        address = "http://localhost:13748";
        domain = "balsoft.ru";
      };
      logging = {
        directory = "./logs";
        file_date_format = "2006-01-02";
        file_mode = 384;
        file_name_format = "{{.Date}}-{{.Index}}.log";
        print_level = "debug";
        timestamp_format = "Jan _2, 2006 15:04:05";
      };
    };
  };

  secrets-envsubst.mautrix-whatsapp-registration = {
    directory = "mautrix-whatsapp";
    secrets = [ "as_token" "hs_token" ];
    owner = "matrix-synapse";
    template = builtins.toJSON {
      as_token = "$as_token";
      hs_token = "$hs_token";
      id = "whatsapp";
      namespaces = {
        users = [{
          exclusive = true;
          regex = "^@whatsapp_[0-9]+:balsoft.ru$";
        }];
      };
      rate_limited = false;
      sender_localpart = "whatsappbot";
      url = "http://localhost:29318";
    };
  };

  systemd.services.mautrix-telegram.serviceConfig.DynamicUser =
    lib.mkForce false;

  systemd.services.mautrix-telegram.serviceConfig.User = "mautrix-telegram";

  users.users.mautrix-telegram.isSystemUser = true;

  users.users.matrix-synapse.name = lib.mkForce "matrix-synapse";
}
