{ pkgs, config, ... }:
{
  home-manager.users.balsoft = {
    home.file.".slack-term".text = builtins.toJSON { 
      emoji = true; 
      key_map.command = {
        "/" = "mode-search"; 
        "<f1>" = "help"; 
        "<next>" = "channel-down"; 
        "<previous>" = "channel-up"; 
        "<up>" = "chat-up"; 
        "<down>" = "chat-down"; 
        C-f = "mode-search"; 
        G = "channel-bottom"; 
        N = "channel-search-previous"; 
        g = "channel-top"; 
        i = "mode-insert"; 
        j = "channel-down"; 
        k = "channel-up"; 
        n = "channel-search-next"; 
        q = "quit"; 
      }; 
      notify = "mention"; 
      slack_token = config.secrets.slack-term; 
      theme = {
        channel = { icon = "fg-green,fg-bold"; prefix = "fg-red,fg-bold"; text = "fg-blue,fg-bold"; };
        message = { name = "colorize,fg-bold"; text = "fg-white"; time = "fg-green,fg-bold"; time_format = "Mon 15:04"; };
      }; 
    };
    xsession.windowManager.i3.config.startup = [
      {
        command = "${config.defaultApplications.term.cmd} -- ${pkgs.slack-term}";
        workspace = "";
      }
    ];
  };
}
