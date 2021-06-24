{
  services.minidlna = {
    enable = true;
    announceInterval = 10;
    mediaDirs = [ "A,/var/lib/music" ];
  };
}
