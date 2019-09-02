{ pkgs, config, lib, ...}:
{
  services.dovecot2 = {
    enable = true;
    sslServerCert = builtins.toFile "balsoft.cert" config.secrets.ssl.cert;
    sslServerKey = builtins.toFile "balsoft.key" config.secrets.ssl.priv;
  };
}
