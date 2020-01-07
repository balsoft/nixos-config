{ pkgs, config, lib, ... }: rec {
  services.autofs.enable = true;
  services.autofs.autoMaster = let
    mapConf = pkgs.writeText "auto" (builtins.concatStringsSep "\n" (map (n:
      "${n} -fstype=fuse,rw,nodev,noatime,allow_other,max_read=65536 :sshfs#balsoft@${n}:")
      ([ "balsoft.ru" ]
        ++ builtins.map (n: "${n}.lan") (builtins.attrNames config.devices))));
  in ''
    /auto file:${mapConf} uid=1000,gid=100,--timeout=30,--ghost
  '';
  environment.etc."autofs.conf".text = services.autofs.autoMaster;

  environment.systemPackages = with pkgs; [ fuse sshfs ];
}
