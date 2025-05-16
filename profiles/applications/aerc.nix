{ config, pkgs, ... }: {
  defaultApplications.mail = {
    cmd = "${pkgs.aerc}/bin/aerc";
    desktop = "aerc";
  };
  startupApplications = [ "${config.defaultApplications.term.cmd} -T aerc -e ${pkgs.aerc}/bin/aerc" ];
  home-manager.users.balsoft = {
    programs.aerc = {
      enable = true;
      extraBinds = {
        global = {
          "<C-p>" = ":prev-tab<Enter>";
          "<C-n>" = ":next-tab<Enter>";
          "?" = ":help keys<Enter>";
        };

        messages = {
          "h" = ":prev-tab<Enter>";
          "l" = ":next-tab<Enter>";

          "j" = ":next<Enter>";
          "<Down>" = ":next<Enter>";
          "<C-d>" = ":next 50%<Enter>";
          "<C-f>" = ":next 100%<Enter>";
          "<PgDn>" = ":next 100%<Enter>";

          "k" = ":prev<Enter>";
          "<Up>" = ":prev<Enter>";
          "<C-u>" = ":prev 50%<Enter>";
          "<C-b>" = ":prev 100%<Enter>";
          "<PgUp>" = ":prev 100%<Enter>";
          "g" = ":select 0<Enter>";
          "G" = ":select -1<Enter>";

          "J" = ":next-folder<Enter>";
          "K" = ":prev-folder<Enter>";
          "H" = ":collapse-folder<Enter>";
          "L" = ":expand-folder<Enter>";

          "v" = ":mark -t<Enter>";
          "x" = ":mark -t<Enter>:next<Enter>";
          "V" = ":mark -v<Enter>";

          "T" = ":toggle-threads<Enter>";

          "<Enter>" = ":view<Enter>";
          "d" = ":prompt 'Really delete this message?' 'delete-message'<Enter>";
          "D" = ":delete<Enter>";
          "A" = ":archive flat<Enter>";

          "C" = ":compose<Enter>";

          "rr" = ":reply -a<Enter>";
          "rq" = ":reply -aq<Enter>";
          "Rr" = ":reply<Enter>";
          "Rq" = ":reply -q<Enter>";

          "c" = ":cf<space>";
          "$" = ":term<space>";
          "!" = ":term<space>";
          "|" = ":pipe<space>";

          "/" = ":search<space>";
          "\\" = ":filter<space>";
          "n" = ":next-result<Enter>";
          "N" = ":prev-result<Enter>";
          "<Esc>" = ":clear<Enter>";
        };

        "messages:folder=Drafts" = { "<Enter>" = ":recall<Enter>"; };

        view = {

          "/" = ":toggle-key-passthrough<Enter>/";
          "q" = ":close<Enter>";
          "O" = ":open<Enter>";
          "S" = ":save<space>";
          "|" = ":pipe<space>";
          "D" = ":delete<Enter>";
          "A" = ":archive flat<Enter>";

          "<C-l>" = ":open-link <space>";

          "f" = ":forward<Enter>";
          "rr" = ":reply -a<Enter>";
          "rq" = ":reply -aq<Enter>";
          "Rr" = ":reply<Enter>";
          "Rq" = ":reply -q<Enter>";

          "H" = ":toggle-headers<Enter>";
          "<C-k>" = ":prev-part<Enter>";
          "<C-j>" = ":next-part<Enter>";
          "J" = ":next<Enter>";
          "K" = ":prev<Enter>";
        };

        "view::passthrough" = {
          "$noinherit" = true;
          "$ex" = "<C-x>";
          "<Esc>" = ":toggle-key-passthrough<Enter>";
        };

        compose = {
          "$noinherit" = "true";
          "$ex" = "<C-x>";
          "<C-k>" = ":prev-field<Enter>";
          "<C-j>" = ":next-field<Enter>";
          "<A-p>" = ":switch-account -p<Enter>";
          "<A-n>" = ":switch-account -n<Enter>";
          "<tab>" = ":next-field<Enter>";
          "<C-p>" = ":prev-tab<Enter>";
          "<C-n>" = ":next-tab<Enter>";
        };

        "compose::editor" = {
          "$noinherit" = "true";
          "$ex" = "<C-x>";
          "<C-k>" = ":prev-field<Enter>";
          "<C-j>" = ":next-field<Enter>";
          "<C-p>" = ":prev-tab<Enter>";
          "<C-n>" = ":next-tab<Enter>";
        };

        "compose::review" = {
          "y" = ":send<Enter>";
          "n" = ":abort<Enter>";
          "p" = ":postpone<Enter>";
          "q" = ":choose -o d discard abort -o p postpone postpone<Enter>";
          "e" = ":edit<Enter>";
          "a" = ":attach<space>";
          "d" = ":detach<space>";
        };

        terminal = {
          "$noinherit" = "true";
          "$ex" = "<C-x>";

          "<C-p>" = ":prev-tab<Enter>";
          "<C-n>" = ":next-tab<Enter>";
        };
      };
      extraConfig = {
        general.unsafe-accounts-conf = true;
        ui = {
          reverse-thread-order = true;
          threading-enabled = true;
          show-thread-context = true;
          this-day-time-format = ''"           15:04"'';
          this-year-time-format = "Mon Jan 02 15:04";
          timestamp-format = "2006-01-02 15:04";

          spinner = "[ ⡿ ],[ ⣟ ],[ ⣯ ],[ ⣷ ],[ ⣾ ],[ ⣽ ],[ ⣻ ],[ ⢿ ]";
          border-char-vertical = "┃";
          border-char-horizontal = "━";
        };
        viewer = { always-show-mime = true; };
        compose = { no-attachment-warning = "^[^>]*attach(ed|ment)"; };
        triggers = {
          email-received = ''exec notify-send --icon=mail-message "Mail from %n" "%s"'';
        };
        filters = {
          "text/plain" = "colorize";
          "text/html" = "html";
          "text/calendar" = "calendar";
          "message/delivery-status" = "colorize";
          "message/rfc822" = "colorize";
          "image/*" = "${pkgs.catimg}/bin/catimg -";
        };
      };
      stylesets = {
        default = {
          "border.bg" = 0;
          "border.fg" = 7;
          "border.reverse" = "false";
          "msglist_default.bg" = 0;
          "msglist_unread.fg" = 3;
          "msglist_unread.bold" = "true";
          "msglist_marked.bg" = 4;
          "dirlist_default.bg" = 0;
          "dirlist_unread.fg" = 3;
        };
      };
    };
    accounts.email.accounts = {
      Personal = {
        primary = true;
        aerc.enable = true;
        realName = "Alexander Bantyev";
        address = "balsoft@balsoft.ru";
        imap.host = "balsoft.ru";
        smtp.host = "balsoft.ru";

        userName = "balsoft@balsoft.ru";
        passwordCommand = "pass email/balsoft@balsoft.ru";

        folders.inbox = "virtual.all";
      };
      Work = {
        aerc.enable = true;
        address = "alexander.bantyev@moduscreate.com";
        realName = "Alexander Bantyev";
        imap.host = "imap.gmail.com";
        smtp.host = "smtp.gmail.com";

        userName = "alexander.bantyev@moduscreate.com";
        passwordCommand = "pass aerc/alexander.bantyev@moduscreate.com";

        folders.inbox = "[Gmail]/All Mail";
      };
    };
  };
}
