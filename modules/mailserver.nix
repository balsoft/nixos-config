{ pkgs, config, lib, ...}:
{
  mailserver = lib.mkIf (config.device == "AMD-Workstation") {
    enable = true;
    fqdn = "balsoft.ru";
    domains = [ "balsoft.ru" ];
    loginAccounts = {
      "balsoft@balsoft.ru" = {
        hashedPassword = "$6$6to2YwlF8id$KZMEf9iRNFJxv4UiXWFgB6AzVk23VvpTmG8UM9ywjmYcUFqcblJRVoz4c3SD1nyRZ0QAXTE727/OPPnyo6Yz2/";
      };
    };
    certificateScheme = 1;
    certificateFile = builtins.toFile "balsoft.crt" config.secrets.ssl.cert;
    keyFile = builtins.toFile "balsoft.key" config.secrets.ssl.priv;
  };
}
