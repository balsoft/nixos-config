{ pkgs, config, lib, inputs, ... }:
let module = toString inputs.simple-nixos-mailserver;
in {
  imports = [ module ];
  secrets.mailserver = {
    owner = "dovecot2:dovecot2";
    services = [ "dovecot" ];
  };
  secrets.mailserver-mastodon = {
    owner = "dovecot2:dovecot2";
    services = [ "dovecot" ];
  };
  services.postfix = {
    dnsBlacklists = [
      "all.s5h.net"
      "b.barracudacentral.org"
      "bl.spamcop.net"
      "blacklist.woody.ch"
      # "bogons.cymru.com"
      # "cbl.abuseat.org"
      # "combined.abuse.ch"
      # "db.wpbl.info"
      # "dnsbl-1.uceprotect.net"
      # "dnsbl-2.uceprotect.net"
      # "dnsbl-3.uceprotect.net"
      # "dnsbl.anticaptcha.net"
      # "dnsbl.dronebl.org"
      # "dnsbl.inps.de"
      # "dnsbl.sorbs.net"
      # "dnsbl.spfbl.net"
      # "drone.abuse.ch"
      # "duinv.aupads.org"
      # "dul.dnsbl.sorbs.net"
      # "dyna.spamrats.com"
      # "dynip.rothen.com"
      # "http.dnsbl.sorbs.net"
      # "ips.backscatterer.org"
      # "ix.dnsbl.manitu.net"
      # "korea.services.net"
      # "misc.dnsbl.sorbs.net"
      # "noptr.spamrats.com"
      # "orvedb.aupads.org"
      # "pbl.spamhaus.org"
      # "proxy.bl.gweep.ca"
      # "psbl.surriel.com"
      # "relays.bl.gweep.ca"
      # "relays.nether.net"
      # "sbl.spamhaus.org"
      # "singular.ttk.pte.hu"
      # "smtp.dnsbl.sorbs.net"
      # "socks.dnsbl.sorbs.net"
      # "spam.abuse.ch"
      # "spam.dnsbl.anonmails.de"
      # "spam.dnsbl.sorbs.net"
      # "spam.spamrats.com"
      # "spambot.bls.digibase.ca"
      # "spamrbl.imp.ch"
      # "spamsources.fabel.dk"
      # "ubl.lashback.com"
      # "ubl.unsubscore.com"
      # "virus.rbl.jp"
      # "web.dnsbl.sorbs.net"
      # "wormrbl.imp.ch"
      # "xbl.spamhaus.org"
      # "z.mailspike.net"
      # "zen.spamhaus.org"
      # "zombie.dnsbl.sorbs.net"
    ];
    dnsBlacklistOverrides = ''
      balsoft.ru OK
      192.168.0.0/16 OK
      ${lib.concatMapStringsSep "\n" (machine: "${machine}.lan OK")
      (builtins.attrNames inputs.self.nixosConfigurations)}
    '';
  };
  services.dovecot2 = {
    mailPlugins.globally.enable = [ "virtual" ];
    extraConfig = ''
      namespace {
        prefix = virtual.
        separator = .
        location = virtual:~/Maildir/virtual
      }
    '';
  };
  systemd.tmpfiles.rules = [
    "d /var/vmail/Maildir 700 virtualMail virtualMail - -"
    "d /var/vmail/Maildir/virtual 700 virtualMail virtualMail - -"
    "d /var/vmail/Maildir/virtual/all 700 virtualMail virtualMail - -"
    "d /var/vmail/Maildir/virtual/INBOX 700 virtualMail virtualMail - -"
    "L+ /var/vmail/Maildir/virtual/all/dovecot-virtual - - - - ${
      pkgs.writeText "virtual.all" ''
        *
          all
      ''
    }"
    "L+ /var/vmail/Maildir/virtual/INBOX/dovecot-virtual - - - - ${
      pkgs.writeText "virtual.INBOX" ''
        virtual.all
          inthread refs x-mailbox INBOX
      ''
    }"
  ];
  mailserver = {
    enable = true;
    stateVersion = 3;
    fqdn = "balsoft.ru";
    domains = [ "balsoft.ru" ];
    mailboxes = {
      Trash = {
        auto = "no";
        specialUse = "Trash";
      };
      Junk = {
        auto = "subscribe";
        specialUse = "Junk";
      };
      Drafts = {
        auto = "subscribe";
        specialUse = "Drafts";
      };
      Sent = {
        auto = "subscribe";
        specialUse = "Sent";
      };
    };
    loginAccounts = {
      "balsoft@balsoft.ru" = {
        aliases = [
          "balsoft"
          "admin@balsoft.ru"
          "patches"
          "patches@balsoft.ru"
          "issues"
          "issues@balsoft.ru"
          "admin"
          "root@balsoft.ru"
          "root"
          "paypal@balsoft.ru"
          "paypal"
        ];
        hashedPasswordFile = config.secrets.mailserver.decrypted;
        sieveScript = ''
          if header :is "X-GitHub-Sender" "serokell-bot" {
            discard;
            stop;
          }
        '';
      };
      "mastodon@balsoft.ru" = {
        aliases = [ "mastodon" ];
        hashedPasswordFile = config.secrets.mailserver-mastodon.decrypted;
      };
    };
    localDnsResolver = false;
    certificateScheme = "manual";
    certificateFile = "/var/lib/acme/balsoft.ru/fullchain.pem";
    keyFile = "/var/lib/acme/balsoft.ru/key.pem";
    enableImap = true;
    enableImapSsl = true;
    virusScanning = false;
  };
}
