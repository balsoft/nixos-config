{ pkgs, config, lib, ... }: rec {
  services.autofs.enable = true;
  services.autofs.autoMaster = let
    mapConf = pkgs.writeText "auto" ''
      balsoft.ru -fstype=fuse,rw,nodev,nonempty,noatime,allow_other,max_read=65536 :sshfs\#balsoft@balsoft.ru\:
    '';
  in ''
    /auto file:${mapConf} uid=1000,gid=100,--timeout=30,--ghost
  '';
}
