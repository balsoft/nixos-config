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
  mailserver = {
    enable = true;
    fqdn = config.secrets.mail.host;
    domains = [ config.secrets.mail.host ];
    loginAccounts = {
      "balsoft@balsoft.ru" = {
        aliases = [ "balsoft"
        "admin@balsoft.ru" "admin"
        "root@balsoft.ru" "root" ];
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
