{pkgs, config, lib, ...}:
with lib;
{
  options.secrets =
  {
    owm-key = mkOption
    {
      type = types.string;
      description = "OpenWeatherMap key";
    };
    irc.password = mkOption
    {
      type = types.string;
      description = "IRC Freenode password";
    };
    gmail.user = mkOption
    {
      type = types.string;
      description = "Gmail user name";
    };
    gmail.password = mkOption
    {
      type = types.string;
      description = "Gmail application password";
    };
    id_rsa = mkOption
    {
      type = types.string;
      description = "SSH RSA private key";
    };
  };
  config =
  {
    secrets = import ../secret.nix;       
  };
}
