{ config, pkgs, ... }: {
  secrets-envsubst.firefox-syncserver = {
    secrets = [
      "SYNC_MASTER_SECRET"
      "SYNC_TOKENSERVER__FXA_METRICS_HASH_SECRET"
    ];
    template = ''
      SYNC_MASTER_SECRET="$SYNC_MASTER_SECRET"
      SYNC_TOKENSERVER__FXA_METRICS_HASH_SECRET="$SYNC_TOKENSERVER__FXA_METRICS_HASH_SECRET"
    '';
  };
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_19;
  };
  services.firefox-syncserver = {
    enable = true;
    singleNode = {
      enable = true;
      enableNginx = true;
      enableTLS = true;
      hostname = "sync.balsoft.eu";
      capacity = 2;
    };
    database.type = "postgresql";
    secrets = toString config.secrets-envsubst.firefox-syncserver;
    settings = {

    };
  };
}
