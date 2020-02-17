{ pkgs, config, lib, ... }:
let thm = config.themes.colors;

in {
  home-manager.users.balsoft = {
    services.dunst = {
      enable = true;
      iconTheme = {
        name = "Papirus-Dark";
        package = pkgs.papirus-icon-theme;
      };
      settings = {
        global = {
          geometry = "500x5-30+50";
          transparency = 10;
          frame_color = thm.blue;
          font = "Roboto 13";
          padding = 15;
          horizontal_padding = 17;
          word_wrap = true;
          follow = "keyboard";
          format = ''
            %s %p %I 
            %b'';
          markup = "full";
        };

        urgency_low = {
          background = thm.bg;
          foreground = thm.fg;
          timeout = 5;
        };

        urgency_normal = {
          background = thm.alt;
          foreground = thm.fg;
          timeout = 10;
        };

        urgency_critical = {
          background = thm.fg;
          foreground = thm.bg;
          timeout = 15;
        };
      };
    };
    xsession.windowManager.i3.config.startup =
    [{ command = "${pkgs.dunst}/bin/dunst"; }];
  };
}
