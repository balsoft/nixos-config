{ pkgs, ... }: {
  home-manager.users.balsoft.wayland.windowManager.sway.config.startup = [{
    command = toString (pkgs.writeShellScript "slack" ''
      librewolf https://tweag.slack.com &
      sleep 5
      swaymsg '[title=Slack.*] move to workspace 󰍩'
      swaymsg '[title=Slack.*] fullscreen disable'
      librewolf https://calendar.google.com
    '');
  }];
}
