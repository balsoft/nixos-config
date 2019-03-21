{ pkgs, config, lib, ... }:
{
  services.mopidy = {
    enable = true;
    extensionPackages =
      with pkgs; 
      [
        mopidy-gmusic
      ];
    configuration = if (! isNull config.secrets.gpmusic) then
      ''
        [gmusic]
        username = ${config.secrets.gpmusic.user}
        password = ${config.secrets.gpmusic.password}
        deviceid = ${config.secrets.gpmusic.deviceid}
        bitrate = 128
      '' else "";

  };
}
