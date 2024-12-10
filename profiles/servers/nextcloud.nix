{ config, pkgs, lib, ... }: {
  services.nextcloud = {
    enable = true;
    hostName = "nextcloud.balsoft.ru";
    config.adminpassFile = config.secrets.nextcloud.decrypted;
    package = pkgs.nextcloud30;
    https = true;
    phpOptions.memory_limit = lib.mkForce "1G";
    extraOptions = {
      "memories.exiftool" = "${lib.getExe pkgs.exiftool}";
      "memories.vod.ffmpeg" = "${lib.getExe pkgs.ffmpeg-headless}";
      "memories.vod.ffprobe" = "${pkgs.ffmpeg-headless}/bin/ffprobe";
    };
    phpExtraExtensions = all: [ ((all.pdlib.override {dlib = (pkgs.dlib.override { blas = pkgs.openblas; });}).overrideAttrs (oa: {buildInputs = oa.buildInputs ++ [  pkgs.openblas pkgs.liblapack];})) ];
  };
  secrets.nextcloud = {
    owner = "nextcloud:nextcloud";
    services = [ "nextcloud-setup" ];
  };
  services.nginx.virtualHosts."nextcloud.balsoft.ru" = {
    enableACME = true;
    forceSSL = true;
    locations."/".proxyWebsockets = true;
  };
}
