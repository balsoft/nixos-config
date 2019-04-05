{pkgs, config, lib, ...}:
with lib;
with types;
let mkCredOption = service: extra: mkOption 
    {
      description = "Credentials for ${service}";
      type = 
      nullOr 
      (submodule
      {
        options = 
        {
          user = mkOption 
          {
            type = string;
            description = "Username for ${service}";
          };
          password = mkOption 
          {
            type = string;
            description = "Password for ${service}";
          };
        } // extra;
      });
    };
in 
rec
{
  options.secrets = {
    owm-key = mkOption
    {
      type = nullOr string;
      description = "OpenWeatherMap key";
    };
    irc = mkCredOption "IRC (konversation)" {};
    gcal = 
    {
      email = mkOption { type = lib.types.string; };
      client-id = mkOption { type = lib.types.string; };
      client-secret = mkOption { type = lib.types.string; };
      refresh-token = mkOption { type = lib.types.string; };
    }; 
    gmail = mkCredOption "gmail (trojita)" {};
    gpmusic = mkCredOption "Google Play Music (mopidy)"
    ({deviceid = mkOption
    {
      type = string;
      description = "Android device ID";
    };});
    rclone = mkOption {
      type = nullOr string;
      description = "Rclone config";
    };
    id_rsa = mkOption
    {
      type = nullOr string;
      description = "SSH RSA private key";
    };
  };
  config =
  let secretnix = import ../secret.nix;
      secrets = if isNull secretnix then 
            mapAttrs (n: v: null) options.secrets
      else secretnix;
  in
  {
    inherit secrets;       
  };
}
