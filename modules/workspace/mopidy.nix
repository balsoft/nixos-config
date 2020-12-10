{ pkgs, config, lib, ... }:
{
  services.mopidy = {
    enable = true;
    extensionPackages = with pkgs; [ mopidy-mpd mopidy-youtube ];
    configuration = ''
      [youtube]
      enabled = true
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
