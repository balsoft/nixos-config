{ pkgs, config, lib, ...}:
{
  mailserver = lib.mkIf (config.device == "AMD-Workstation") {
    enable = true;
    fqdn = "balsoft.ru";
    domains = [ "balsoft.ru" ];
    loginAccounts = {
      "balsoft@balsoft.ru" = {
        hashedPassword = "$6$MZBL0bX51v$Qq5kxLxo60pqCqCcL/YibjOYnkt4OgY41pjPIYM6CvXB41nygy5HihvjdGjxnDIyz.M47LfcPMRGK0NfK1xe60";
      };
    };
    certificateScheme = 1;
    certificateFile = builtins.toFile "balsoft.crt" config.secrets.ssl.cert;
    keyFile = builtins.toFile "balsoft.key" config.secrets.ssl.priv;
    enableImap = true;
    enableImapSsl = true;
    virusScanning = false;
  };

  
}
