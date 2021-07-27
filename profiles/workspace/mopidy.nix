{ pkgs, config, lib, ... }:
{
  secrets.mopidy_ytmusic_auth = {
    owner = "mopidy:mopidy";
    services = [ "mopidy" ];
  };

  services.mopidy = {
    enable = true;
    extensionPackages = with pkgs; [ mopidy-mpd mopidy-ytmusic ];
    configuration = ''
      [ytmusic]
      enabled=true
      auth_json=${config.secrets.mopidy_ytmusic_auth.decrypted}
      [mpd]
      hostname = 0.0.0.0
      port = 6600
      [audio]
      output = pulsesink server=127.0.0.1
    '';
  };

  systemd.services.mopidy = {
    after = [ "network-online.target" ];
  };

}
