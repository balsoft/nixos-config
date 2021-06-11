{ pkgs, lib, config, inputs, ... }: {

  environment.sessionVariables =
    config.home-manager.users.balsoft.home.sessionVariables // rec {
      LESS = "MR";
      SYSTEMD_LESS = LESS;
    };

  home-manager.users.balsoft = {
    news.display = "silent";

    systemd.user.startServices = true;

    home.stateVersion = "20.09";
  };

  persist.cache.directories = [ "/home/balsoft/.cache" "/var/cache" ];

  persist.state.directories = [ "/var/lib/nixos" "/var/lib/systemd" ];

  system.stateVersion = "18.03";

  systemd.services.systemd-timesyncd.wantedBy = [ "multi-user.target" ];

  systemd.timers.systemd-timesyncd = { timerConfig.OnCalendar = "hourly"; };

  services.avahi.enable = true;
}
