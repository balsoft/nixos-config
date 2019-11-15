{pkgs, config, lib, ...}:
{
  services.autofs.enable = true;

  services.autofs.autoMaster = ''
    * -fstype=fuse,rw,nodev,nonempty,noatime,allow_other,max_read=65536,uid=1000,gid=100,--timeout=30,--ghost :sshfs\#balsoft@&\:
  '';
}
