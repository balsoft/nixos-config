{ config, pkgs, lib, ... }:
with import ../../support.nix { inherit lib config; }; let
  gearyConfig = {
    Account = {
      label = "";
      ordinal = 6;
      prefetch_days = -1;
      save_drafts = true;
      save_sent = true;
      sender_mailboxes = "Alexander Bantyev <balsoft@balsoft.ru>;";
      service_provider = "other";
      signature = builtins.replaceStrings ["\n"] ["\\n"] ''
        --
        Александр Бантьев /Alexander Bantyev/ aka balsoft

        Nix DevOPS/SRE at serokell.io

        <balsoft@balsoft.ru>
        <alexander.bantyev@serokell.io>

        matrix://@balsoft:balsoft.ru
        (https://matrix.to/#/@balsoft:balsoft.ru)
        https://t.me/balsoft
        https://github.com/balsoft
      '';
      use_signature = true;
    };
    Folders = {
      archive_folder = "Archive;";
      drafts_folder = "";
      junk_folder = "";
      sent_folder = "";
      trash_folder = "";
    };
    Incoming = {
      credentials = "custom";
      host = "balsoft.ru";
      login = "balsoft@balsoft.ru";
      port = 993;
      remember_password = true;
      transport_security = "transport";
    };
    Metadata = {
      status = "enabled";
      version = 1;
    };
    Outgoing = {
      credentials = "use-incoming";
      host = "balsoft.ru";
      port = 587;
      remember_password = true;
      transport_security = "start-tls";
    };
  };
in {
  programs.geary.enable = true;
  home-manager.users.balsoft = {
    xdg.configFile."geary/user-style.css".text = ''
      *, html, body, body.plain div, body.plain a, body.plain p, body.plain span {
        background: ${config.themes.colors .bg} !important;
        color: ${config.themes.colors.fg} !important;
        font-family: 'IBM Plex Mono', monospace !important;
      }
      *, html, body {
        font-size: 16px;
      }
    '';
    xdg.configFile."geary/account_03/geary.ini".text = genIni gearyConfig;
  };
}
