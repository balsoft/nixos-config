{ pkgs, config, ... }:
let
  weechat = pkgs.weechat.override {
    configure = { availablePlugins, ... }: {
      scripts = with pkgs.weechatScripts; [ wee-slack weechat-matrix-bridge ];
    };
  };
in {
  home-manager.users.balsoft = {
    home.file.".weechat/python/autoload/notify_send.py".source =
      "${(import ../../nix/sources.nix).weechat-notify-send}/notify_send.py";

    home.file.".weechat/perl/autoload/multiline.pl".source =
      "${pkgs.imports.scripts}/perl/multiline.pl";

    home.file.".weechat/plugins.conf".text = ''
      [var]
      lua.matrix.autojoin_on_invite = "on"
      lua.matrix.backlog_lines = "120"
      lua.matrix.debug = "off"
      lua.matrix.encrypted_message_color = "lightgreen"
      lua.matrix.homeserver_url = "https://matrix.balsoft.ru/"
      lua.matrix.local_echo = "on"
      lua.matrix.nick_style = "nick"
      lua.matrix.password = "${config.secrets.matrix.password}"
      lua.matrix.presence_filter = "on"
      lua.matrix.read_receipts = "on"
      lua.matrix.timeout = "20"
      lua.matrix.typing_notices = "on"
      lua.matrix.user = "${config.secrets.matrix.user}"
      python.slack.auto_open_threads = "true"
      python.slack.background_load_all_history = "true"
      python.slack.channel_name_typing_indicator = "true"
      python.slack.color_buflist_muted_channels = "darkgray"
      python.slack.color_edited_suffix = "095"
      python.slack.color_reaction_suffix = "darkgray"
      python.slack.color_thread_suffix = "lightcyan"
      python.slack.colorize_private_chats = "false"
      python.slack.debug_level = "3"
      python.slack.debug_mode = "false"
      python.slack.distracting_channels = ""
      python.slack.external_user_suffix = "*"
      python.slack.files_download_location = "/home/balsoft/Downloads/slack"
      python.slack.group_name_prefix = "&"
      python.slack.map_underline_to = "_"
      python.slack.migrated = "true"
      python.slack.muted_channels_activity = "personal_highlights"
      python.slack.never_away = "false"
      python.slack.notify_usergroup_handle_updated = "false"
      python.slack.record_events = "false"
      python.slack.render_bold_as = "bold"
      python.slack.render_italic_as = "italic"
      python.slack.send_typing_notice = "true"
      python.slack.server_aliases = ""
      python.slack.shared_name_prefix = "%"
      python.slack.short_buffer_names = "false"
      python.slack.show_buflist_presence = "true"
      python.slack.show_reaction_nicks = "true"
      python.slack.slack_api_token = "${config.secrets.slack-term}"
      python.slack.slack_timeout = "20000"
      python.slack.switch_buffer_on_join = "true"
      python.slack.thread_messages_in_channel = "false"
      python.slack.unfurl_auto_link_display = "both"
      python.slack.unfurl_ignore_alt_text = "false"
      python.slack.unhide_buffers_with_activity = "false"
    '';

    home.packages = [ weechat ];
    xsession.windowManager.i3.config.startup = [{
      command =
        "${config.defaultApplications.term.cmd} -e ${weechat}/bin/weechat";
    }];
  };
}
