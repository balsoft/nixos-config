{ pkgs, config, lib, ...}:
with import ../../support.nix {inherit lib config;};
{
  home-manager.users.balsoft.xsession.windowManager.i3.config.startup = [ {command = "${pkgs.konversation}/bin/konversation";} ];
  home-manager.users.balsoft.xdg.configFile."konversationrc".text = if ! isNull config.secrets.irc then genIni
  {
    Appearance =
    {
      AllowColorCodes = false;
      ChannelSplitterSizes = "961,173";
      NickColor0 = thmDec.fg;
      NickColor1 = thmDec.red;
      NickColor2 = thmDec.orange;
      NickColor3 = thmDec.yellow;
      NickColor4 = thmDec.cyan;
      NickColor5 = thmDec.purple;
      NickColor6 = thmDec.blue;
      NickColor7 = thmDec.green;
      NickColor8 = thmDec.fg;
      TopicSplitterSizes = "28,880";
      TreeSplitterSizes = "145,483";
    };
    "Identity 0" =
    {
      AuthType = "nickserv";
      AutomaticAway = false;
      AutomaticUnaway = false;
      AwayInactivity = 10;
      AwayMessage = "";
      AwayNick = "";
      AwayReason = "Gone away for now";
      Bot = "nickserv";
      Codec = "UTF-8";
      Ident = config.secrets.irc.user;
      InsertRememberLineOnAway = false;
      KickReason = "User terminated!";
      Name = "Default Identity";
      Nicknames = config.secrets.irc.user;
      NickservCommand = "identify";
      PartReason = "Konversation terminated!";
      Password = config.secrets.irc.password;
      PemClientCertFile = "";
      PreShellCommand = "";
      QuitReason = "Konversation terminated!";
      Realname = config.secrets.irc.user;
      ReturnMessage = "";
      SaslAccount = "";
      ShowAwayMessage = false;
    };
    
    "Message Text Colors" =
    {
      ActionMessage = thmDec.orange;
      ChannelMessage = thmDec.purple;
      CommandMessage = thmDec.yellow;
      Hyperlink = thmDec.blue;
      QueryMessage = thmDec.fg;
      ServerMessage = thmDec.red;
      TextViewBackground = thmDec.bg;
      Time = thmDec.alt;
    };
    "Server 0" =
    {
      BypassProxy = false;
      Password = "";
      Port = 7000;
      SSLEnabled = true;
      Server = "chat.freenode.net";
    };
    "ServerGroup 0" =
    {
      AutoConnect = true;
      EnableNotifications = true;
      Identity = "Default Identity";
      Name = "freenode";
      ServerList = "Server 0";
    };
  } else "";
}
