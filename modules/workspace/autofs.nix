{ pkgs, config, lib, ... }: rec {
  services.autofs.enable = true;
  services.autofs.autoMaster = let
    mapConf = pkgs.writeText "auto" ''
      * -fstype=fuse,rw,nodev,noatime,allow_other,max_read=65536 :${pkgs.sshfs}/bin/sshfs\#balsofttrash@&\:
    '';
  in ''
    /auto file:${mapConf} uid=1000,gid=100,--timeout=30,--ghost
  '';

  environment.systemPackages = with pkgs; [ fuse ];
}