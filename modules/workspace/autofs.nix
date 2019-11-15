{pkgs, config, lib, ...}:
rec {
  services.autofs.enable = true;
  services.autofs.autoMaster = environment.etc."auto.master".text;
  environment.etc."auto.master".text = let
    mapConf = pkgs.writeText "auto" ''
      * -fstype=fuse,rw,nodev,nonempty,noatime,allow_other,max_read=65536 :sshfs\#balsoft@&\:
    '';
  in ''
    /auto file:${mapConf} uid=1000,gid=100,--timeout=30,--ghost
  '';
}
