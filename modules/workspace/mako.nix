{ pkgs, config, ...}:
{
  home-manager.users.balsoft.programs.mako = {
    enable = true;
    layer = "overlay";
    font = "Roboto 13";
    width = 500;
    height = 80;
    defaultTimeout = 10000;
    maxVisible = 10;
    backgroundColor = config.themes.colors.bg;
    textColor = config.themes.colors.fg;
    borderColor = config.themes.colors.blue;
    progressColor = "over ${config.themes.colors.green}";
  };
}
