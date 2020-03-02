{ pkgs, config, lib, ... }:
let
  module = toString (import ../nix/sources.nix).simple-nixos-mailserver;
  readCommandResult = command:
    builtins.readFile (pkgs.runCommand "cmd" { preferLocalBuild = true; }
      "echo -n $(${command}) > $out");
  hashedPassword = readCommandResult
    "${pkgs.mkpasswd}/bin/mkpasswd -m sha-512 '${config.secrets.mail.password}'";
in {
  imports = [ module ];
  services.postfix = {
    dnsBlacklists = [
      "all.s5h.net"
      "b.barracudacentral.org"
      "bl.spamcop.net"
      "blacklist.woody.ch"
      "bogons.cymru.com"
      "cbl.abuseat.org"
      "combined.abuse.ch"
      "db.wpbl.info"
      "dnsbl-1.uceprotect.net"
      "dnsbl-2.uceprotect.net"
      "dnsbl-3.uceprotect.net"
      "dnsbl.anticaptcha.net"
      "dnsbl.dronebl.org"
      "dnsbl.inps.de"
      "dnsbl.sorbs.net"
      "dnsbl.spfbl.net"
      "drone.abuse.ch"
      "duinv.aupads.org"
      "dul.dnsbl.sorbs.net"
      "dyna.spamrats.com"
      "dynip.rothen.com"
      "http.dnsbl.sorbs.net"
      "ips.backscatterer.org"
      "ix.dnsbl.manitu.net"
      "korea.services.net"
      "misc.dnsbl.sorbs.net"
      "noptr.spamrats.com"
      "orvedb.aupads.org"
      "pbl.spamhaus.org"
      "proxy.bl.gweep.ca"
      "psbl.surriel.com"
      "relays.bl.gweep.ca"
      "relays.nether.net"
      "sbl.spamhaus.org"
      "singular.ttk.pte.hu"
      "smtp.dnsbl.sorbs.net"
      "socks.dnsbl.sorbs.net"
      "spam.abuse.ch"
      "spam.dnsbl.anonmails.de"
      "spam.dnsbl.sorbs.net"
      "spam.spamrats.com"
      "spambot.bls.digibase.ca"
      "spamrbl.imp.ch"
      "spamsources.fabel.dk"
      "ubl.lashback.com"
      "ubl.unsubscore.com"
      "virus.rbl.jp"
      "web.dnsbl.sorbs.net"
      "wormrbl.imp.ch"
      "xbl.spamhaus.org"
      "z.mailspike.net"
      "zen.spamhaus.org"
      "zombie.dnsbl.sorbs.net"
    ];
    networks = [ "192.168.1.1/24" ];
  };
  mailserver = {
    enable = true;
    fqdn = config.secrets.mail.host;
    domains = [ config.secrets.mail.host ];
    loginAccounts = {
      "balsoft@balsoft.ru" = {
        aliases =
          [ "balsoft" "admin@balsoft.ru" "admin" "root@balsoft.ru" "root" ];
        inherit hashedPassword;
      };
    };
    localDnsResolver = false;
    certificateScheme = 1;
    certificateFile = builtins.toFile "balsoft.crt" config.secrets.ssl.cert;
    keyFile = builtins.toFile "balsoft.key" config.secrets.ssl.priv;
    enableImap = true;
    enableImapSsl = true;
    virusScanning = false;
  };
}
