{ pkgs, config, lib, ... }: rec {
  services.autofs.enable = true;
  services.autofs.autoMaster = "";
  environment.etc."autofs.conf".text = let
    mapConf = pkgs.writeText "auto" (builtins.concatStringsSep "\n" (map (n:
      "${n} -fstype=fuse,rw,nodev,noatime,allow_other,max_read=65536 :${pkgs.sshfs}/bin/sshfs#balsoft@${n}:")
      ([ "balsoft.ru" ] ++ builtins.map (n: "${n}.lan") (builtins.attrNames config.devices))));
  in ''
    /auto file:${mapConf} uid=1000,gid=100,--timeout=30,--ghost
  '';

  environment.systemPackages = with pkgs; [ fuse ];
}
