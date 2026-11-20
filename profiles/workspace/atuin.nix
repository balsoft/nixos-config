{ config, pkgs, ... }: {
  environment.sessionVariables = {
    ATUIN_HOME = "${config.home-manager.users.balsoft.xdg.dataHome}/atuin";
    ATUIN_CONFIG_DIR = "${config.home-manager.users.balsoft.xdg.configHome}/atuin";
  };
  persist.derivative.homeDirectories = [ ".local/share/atuin" ];
  home-manager.users.balsoft = {
    systemd.user.services.atuin-setup = {
      Install.WantedBy = [ "sway-session.target" ];
      Service = {
        Environment = [
          "GPG_TTY=/dev/tty1"
          "GNUPGHOME=${config.home-manager.users.balsoft.xdg.dataHome}/gnupg"
          "PATH=${pkgs.lib.makeBinPath [
            config.home-manager.users.balsoft.programs.atuin.package
            pkgs.pass
          ]}"
          "ATUIN_HOME=${config.home-manager.users.balsoft.xdg.dataHome}/atuin"
          "ATUIN_CONFIG_DIR=${config.home-manager.users.balsoft.xdg.configHome}/atuin"
        ];
        Type = "oneshot";
        ExecStart = pkgs.writeShellScript "atuin-setup" ''
          atuin login \
            -u balsoft \
            -p "$(pass atuin.balsoft.eu/balsoft)" \
            -k "$(pass atuin.balsoft.eu/key)"
          atuin sync
        '';
      };
    };
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
