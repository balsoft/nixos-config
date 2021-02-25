{ pkgs, config, ... }: {
  home-manager.users.balsoft = {
    wayland.windowManager.sway.config.startup = [{ command = "mako"; }];
    programs.mako = {
      enable = true;
      layer = "overlay";
      font = "IBM Plex 13";
      width = 500;
      height = 80;
      defaultTimeout = 10000;
      maxVisible = 10;
      backgroundColor = "${config.themes.colors.bg}AA";
      textColor = config.themes.colors.fg;
      borderColor = "${config.themes.colors.blue}AA";
      progressColor = "over ${config.themes.colors.green}";
      iconPath = "${pkgs.papirus-icon-theme}/share/icons/Papirus-Dark";
      maxIconSize = 24;
    };
  };
}
