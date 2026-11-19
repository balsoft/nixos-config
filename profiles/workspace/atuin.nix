{ config, pkgs, ... }: {
  environment.sessionVariables = {
    ATUIN_HOME = "${config.home-manager.users.balsoft.xdg.dataHome}/atuin";
    ATUIN_CONFIG_DIR = "${config.home-manager.users.balsoft.xdg.configHome}/atuin";
  };
  persist.derivative.homeDirectories = [ ".local/share/atuin" ];
  home-manager.users.balsoft = {
    home.activation.atuin = ''
      ${config.home-manager.users.balsoft.programs.atuin.package}/bin/atuin login \
        -u balsoft \
        -p "$(${pkgs.pass}/bin/pass atuin.balsoft.eu/balsoft)" \
        -k "$(${pkgs.pass}/bin/pass atuin.balsoft.eu/key)"
      ${config.home-manager.users.balsoft.programs.atuin.package}/bin/atuin sync
    '';
    programs.atuin = {
      enable = true;
      enableZshIntegration = true;
      enableBashIntegration = true;
      daemon.enable = true;
      forceOverwriteSettings = true;
      flags = [ "--disable-up-arrow" ];
      settings = {
        auto_sync = true;
        sync_frequency = "1m";
        sync_address = "https://atuin.balsoft.eu";
      };
    };
    programs.zsh.initContent = "export ZSH_AUTOSUGGEST_STRATEGY=(atuin)";
  };
}
