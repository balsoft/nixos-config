{ pkgs, config, lib, ... }:
{
  services.mopidy = {
    enable = true;
    extensionPackages = with pkgs; [ mopidy-mpd mopidy-gmusic ];
    configuration = (if (!isNull config.secrets.gpmusic) then ''
      [gmusic]
      username = ${config.secrets.gpmusic.user}
      password = ${config.secrets.gpmusic.password}
      deviceid = ${config.secrets.gpmusic.deviceid}
      bitrate = 128
    '' else
    "") + ''
      [mpd]
      hostname = 0.0.0.0
      [audio]
      output = pulsesink server=127.0.0.1
    '';
  };

  systemd.services.mopidy = {
    after = [ "network-online.target" ];
  };

}
