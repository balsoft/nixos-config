{ pkgs, config, ... }:
{
  home-manager.users.balsoft = {
    home.file.".slack-term".text = builtins.toJSON { 
      emoji = true; 
      key_map = { 
        "<up>" = "chat-up";
        "<down>" = "chat-down";
        "<previous>" = "channel-up";
        "<next>" = "channel-down";
      }; 
      notify = "mention"; 
      slack_token = config.secrets.slack-term; 
      theme = {
        channel = { icon = "fg-green,fg-bold"; prefix = "fg-red,fg-bold"; text = "fg-blue,fg-bold"; };
        message = { name = "colorize,fg-bold"; text = "fg-white"; time = "fg-green,fg-bold"; time_format = "Mon 15:04"; };
      }; 
    };
    xsession.windowManager.i3.startup = [
      {
        command = "${config.defaultApplications.term} -- ${pkgs.slack-term}";
        workspace = "";
      }
    ];
  };
}
