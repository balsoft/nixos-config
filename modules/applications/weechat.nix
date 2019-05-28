{ pkgs, config, ... }:
let 
weechat = pkgs.weechat.override {
  configure = {availablePlugins, ...}: 
  {
    /*
    plugins = with availablePlugins; [
    (python.withPackages (ps: with ps; [ websocket_client websocket]))
    ];
    */
    scripts = with pkgs.weechatScripts; [wee-slack];
  };
};
in
{
  home-manager.users.balsoft = {
    home.file.".weechat/plugins.conf".text = ''
      [var]
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

      [desc]
      python.slack.auto_open_threads = "Automatically open threads when mentioned or inresponse to own messages."
      python.slack.background_load_all_history = "Load history for each channel in the background as soon as it opens, rather than waiting for the user to look at it."
      python.slack.channel_name_typing_indicator = "Change the prefix of a channel from # to > when someone is typing in it. Note that this will (temporarily) affect the sort order if you sort buffers by name rather than by number."
      python.slack.color_buflist_muted_channels = "Color to use for muted channels in the buflist"
      python.slack.color_edited_suffix = "Color to use for (edited) suffix on messages that have been edited."
      python.slack.color_reaction_suffix = "Color to use for the [:wave:(@user)] suffix on messages that have reactions attached to them."
      python.slack.color_thread_suffix = "Color to use for the [thread: XXX] suffix on messages that have threads attached to them."
      python.slack.colorize_private_chats = "Whether to use nick-colors in DM windows."
      python.slack.debug_level = "Show only this level of debug info (or higher) when debug_mode is on. Lower levels -> more messages."
      python.slack.debug_mode = "Open a dedicated buffer for debug messages and start logging to it. How verbose the logging is depends on log_level."
      python.slack.distracting_channels = "List of channels to hide."
      python.slack.external_user_suffix = "The suffix appended to nicks to indicate external users."
      python.slack.files_download_location = "If set, file attachments will be automatically downloaded to this location."
      python.slack.group_name_prefix = "The prefix of buffer names for groups (private channels)."
      python.slack.map_underline_to = "When sending underlined text to slack, use this formatting character for it. The default ("_") sends it as italics. Use "*" to send bold instead."
      python.slack.muted_channels_activity = "Control which activity you see from muted channels, either none, personal_highlights, all_highlights or all. none: Don't show any activity. personal_highlights: Only show personal highlights, i.e. not @channel and @here. all_highlights: Show all highlights, but not other messages. all: Show all activity, like other channels."
      python.slack.never_away = "Poke Slack every five minutes so that it never marks you "away"."
      python.slack.notify_usergroup_handle_updated = "Control if you want to see notification when a usergroup's handle has changed, either true or false"
      python.slack.record_events = "Log all traffic from Slack to disk as JSON."
      python.slack.render_bold_as = "When receiving bold text from Slack, render it as this in weechat."
      python.slack.render_italic_as = "When receiving bold text from Slack, render it as this in weechat. If your terminal lacks italic support, consider using "underline" instead."
      python.slack.send_typing_notice = "Alert Slack users when you are typing a message in the input bar (Requires reload)"
      python.slack.server_aliases = "A comma separated list of `subdomain:alias` pairs. The alias will be used instead of the actual name of the slack (in buffer names, logging, etc). E.g `work:no_fun_allowed` would make your work slack show up as `no_fun_allowed` rather than `work.slack.com`."
      python.slack.shared_name_prefix = "The prefix of buffer names for shared channels."
      python.slack.short_buffer_names = "Use `foo.#channel` rather than `foo.slack.com.#channel` as the internal name for Slack buffers."
      python.slack.show_buflist_presence = "Display a `+` character in the buffer list for present users."
      python.slack.show_reaction_nicks = "Display the name of the reacting user(s) alongside each reactji."
      python.slack.slack_api_token = "List of Slack API tokens, one per Slack instance you want to connect to. See the README for details on how to get these."
      python.slack.slack_timeout = "How long (ms) to wait when communicating with Slack."
      python.slack.switch_buffer_on_join = "When /joining a channel, automatically switch to it as well."
      python.slack.thread_messages_in_channel = "When enabled shows thread messages in the parent channel."
      python.slack.unfurl_auto_link_display = "When displaying ("unfurling") links to channels/users/etc, determine what is displayed when the text matches the url without the protocol. This happens when Slack automatically creates links, e.g. from words separated by dots or email addresses. Set it to "text" to only display the text written by the user, "url" to only display the url or "both" (the default) to display both."
      python.slack.unfurl_ignore_alt_text = "When displaying ("unfurling") links to channels/users/etc, ignore the "alt text" present in the message and instead use the canonical name of the thing being linked to."
      python.slack.unhide_buffers_with_activity = "When activity occurs on a buffer, unhide it even if it was previously hidden (whether by the user or by the distracting_channels setting)."
    '';
    home.packages = [weechat];
    xsession.windowManager.i3.config.startup = [{
      command =
        "konsole -e ${weechat}/bin/weechat";
    }];
  };
}
