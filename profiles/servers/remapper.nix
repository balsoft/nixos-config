{ config, pkgs, ... }: {
  systemd.services.remapper = {
    path = [ pkgs.remapper ];
    script = "remapper";
    serviceConfig = {
      EnvironmentFile = config.secrets.remapper-telegram-token.decrypted;
      PrivateTmp = true;
      User = "remapper";
      Group = "remapper";
    };
  };
  users.users.remapper = {
    isSystemUser = true;
    group = "remapper";
  };
  users.groups.remapper = { };
  secrets.remapper-telegram-token = {
    owner = "remapper:remapper";
    services = [ "remapper" ];
  };
}