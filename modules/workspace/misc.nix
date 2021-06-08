{ pkgs, lib, config, inputs, ... }: {
  systemd.coredump.enable = true;

  environment.sessionVariables =
    config.home-manager.users.balsoft.home.sessionVariables // rec {
      NIX_AUTO_RUN = "1";
      LESS = "MR";
      SYSTEMD_LESS = LESS;
      DE = "generic";
    };

  home-manager.users.balsoft = {
    xdg.enable = true;

    home.activation."mimeapps-remove" = {
      before = [ "linkGeneration" ];
      after = [ ];
      data = "rm -f /home/balsoft/.config/mimeapps.list";
    };

    news.display = "silent";
    programs.command-not-found = {
      enable = true;
      dbPath = ../../misc/programs.sqlite;
    };
    systemd.user.startServices = true;

    programs.direnv.enable = true;
    programs.direnv.enableNixDirenvIntegration = true;

    home.stateVersion = "20.09";
  };

  persist.cache.directories = [ "/home/balsoft/.cache" "/var/cache" ];

  persist.state.directories =
    [ "/home/balsoft/.local/share/direnv" "/var/lib/nixos" "/var/lib/systemd" ];

  services.avahi.enable = true;

  system.stateVersion = "18.03";

}
