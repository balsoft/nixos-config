{ config, pkgs, ... }: {
  defaultApplications.term = {
    cmd = "${pkgs.kitty}/bin/kitty";
    desktop = "kitty";
  };
  home-manager.users.balsoft = {
    programs.kitty = {
      enable = true;
      shellIntegration.enableZshIntegration = true;
      actionAliases.launch_window = "launch --cwd=current --type=os-window";
      keybindings = {
        "ctrl+Enter" = "launch_window";
        "ctrl+shift+f" = "pipe @screen_scrollback overlay hx";
      };
      settings = with pkgs.my-lib.thmHash config.themes.colors; {
        font_family = config.themes.fonts.mono.family;
        font_size = config.themes.fonts.mono.size;

        shell = "${pkgs.zsh}/bin/zsh";

        cursor_shape = "beam";
        cursor = base05;
        cursor_text_color = base02;

        background = base00;
        foreground = base05;
        selection_background = "none";
        selection_foreground = "none";
        color0 = base00;
        color1 = base08;
        color2 = base0B;
        color3 = base0A;
        color4 = base0D;
        color5 = base0E;
        color6 = base07;
        color7 = base05;

      };
    };
  };

}
