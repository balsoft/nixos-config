{ ... }: {
  services.ergochat = {
    enable = true;
    settings = {
      accounts = {
        authentication-enabled = true;
        multiclient = {
          allowed-by-default = true;
          always-on = "opt-out";
          auto-away = "opt-out";
          enabled = true;
        };
        registration = {
          allow-before-connect = true;
          bcrypt-cost = 4;
          email-verification = { enabled = false; };
          enabled = true;
          throttling = {
            duration = "10m";
            enabled = true;
            max-attempts = 30;
          };
        };
      };
      channels = {
        default-modes = "+ntC";
        registration = { enabled = true; };
      };
      datastore = {
        autoupgrade = true;
        path = "/var/lib/ergo/ircd.db";
      };
      history = {
        autoreplay-on-join = 0;
        autoresize-window = "3d";
        channel-length = 2048;
        chathistory-maxmessages = 100;
        client-length = 256;
        enabled = true;
        restrictions = {
          expire-time = "1w";
          grace-period = "1h";
          query-cutoff = "none";
        };
        retention = {
          allow-individual-delete = false;
          enable-account-indexing = false;
        };
        tagmsg-storage = {
          default = false;
          whitelist = [ "+draft/react" "+react" ];
        };
        znc-maxmessages = 2048;
      };
      limits = {
        awaylen = 390;
        channellen = 64;
        identlen = 20;
        kicklen = 390;
        nicklen = 32;
        topiclen = 390;
      };
      network = { name = "balsoftnet"; };
      server = {
        casemapping = "permissive";
        check-ident = false;
        enforce-utf = true;
        forward-confirm-hostnames = false;
        ip-cloaking = { enabled = false; };
        ip-limits = {
          count = false;
          throttle = false;
        };
        listeners = { ":6667" = { }; };
        lookup-hostnames = false;
        max-sendq = "1M";
        name = "balsoft.ru";
        relaymsg = { enabled = false; };
      };
    };
  };
  services.thelounge = {
    enable = true;
    extraConfig = {
      host = "localhost";
      reverseProxy = true;
      lockNetwork = true;
      defaults = {
        host = "localhost";
        name = "balsoftnet";
        port = 6667;
        tls = false;
        nick = "user%%%";
        leaveMessage = "<3";
        join = "#klananas";
      };
    };
    port = 9857;
    public = true;
  };
  services.nginx.virtualHosts."chat.balsoft.ru" = {
    forceSSL = true;
    enableACME = true;
    locations."/".proxyPass = "http://localhost:9857";
    basicAuthFile = "/var/lib/chat.passwd";
  };
}
