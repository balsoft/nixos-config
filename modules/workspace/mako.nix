{ pkgs, config, ...}:
{
  home-manager.users.balsoft.xsession.windowManager.i3.config.startup = [ { command = "${pkgs.mako}/bin/mako --layer overlay --font 'Roboto 13' --width 500 --height 80 --background-color '${config.themes.colors.bg}' --text-color '${config.themes.colors.fg}' --border-color '${config.themes.colors.blue}'"; } ];
}
