{ config, pkgs, ... }: {

  secrets.shadowsocks_options = {
    owner = "shadowsocks:shadowsocks";
    services = [ "shadowsocks" ];
  };

  users.users.shadowsocks = {
    isSystemUser = true;
    group = "shadowsocks";
  };

  users.groups.shadowsocks = { };

  systemd.services.shadowsocks = {
    script = "ss-local -l 5555 $(cat ${config.secrets.shadowsocks_options})";
    path = [ pkgs.shadowsocks-libev ];
    serviceConfig = {
      User = "shadowsocks";
      Group = "shadowsocks";
    };
    wantedBy = [ "multi-user.target" ];
  };
}
