{ pkgs, config, lib, ... }:
let
  readCommandResult = command:
  builtins.readFile (pkgs.runCommand "cmd" { } "${command} > $out");
  
  hashedPassword = readCommandResult "${pkgs.mkpasswd}/bin/mkpasswd -m sha512crypt <<< '${config.secrets.mail.password}'";
in {
  mailserver = lib.mkIf (config.device == "AMD-Workstation") {
    enable = true;
    fqdn = config.secrets.mail.host;
    domains = [ config.secrets.mail.host ];
    loginAccounts = {
      "balsoft@balsoft.ru" = { inherit hashedPassword; };
    };
    certificateScheme = 1;
    certificateFile = builtins.toFile "balsoft.crt" config.secrets.ssl.cert;
    keyFile = builtins.toFile "balsoft.key" config.secrets.ssl.priv;
    enableImap = true;
    enableImapSsl = true;
    virusScanning = false;
  };
}
