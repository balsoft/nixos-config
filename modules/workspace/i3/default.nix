{ pkgs, lib, config, ... }:
let
  thm = config.themes.colors;
  apps = config.defaultApplications;
  lock_fork = "swaylock -c ${builtins.substring 1 7 thm.bg}";
  lock = pkgs.writeShellScript "lock" "swaymsg 'output * dpms off'; swaylock -c ${builtins.substring 1 7 thm.bg}; swaymsg 'output * dpms on'";
in {
  environment.sessionVariables._JAVA_AWT_WM_NONREPARENTING = "1";

  home-manager.users.balsoft.xsession.windowManager.i3 = {
    enable = true;
    package = pkgs.i3-gaps;
    config = rec {
      assigns = {
        "" = [ { class = "Chromium"; } { app_id = "firefox"; } { class = "Firefox"; } ];
        "" = [
          { app_id = "org.kde.trojita"; }
          { title = ".* - Sylpheed.*"; }
          { title = "balsoft : weechat.*"; }
          { title = "nheko"; }
          { title = "Slack"; }
        ];
        "ﱘ" = [{ app_id = "cantata"; }];
      };
      fonts = [ "RobotoMono 9" ];

      bars = [ ];

      colors = rec {
        background = thm.bg;
        unfocused = {
          text = thm.alt;
          border = thm.dark;
          background = thm.bg;
          childBorder = thm.dark;
          indicator = thm.fg;
        };
        focusedInactive = unfocused;
        urgent = unfocused // {
          text = thm.fg;
          border = thm.orange;
          childBorder = thm.orange;
        };
        focused = unfocused // {
          childBorder = thm.blue;
          border = thm.blue;
          background = thm.dark;
          text = thm.fg;
        };
      };
      gaps = {
        inner = 6;
        smartGaps = true;
        smartBorders = "on";
      };
      focus.followMouse = false;
      modifier = "Mod4";
      window = {
        border = 1;
        titlebar = true;
        commands = [
          {
            command = "border pixel 2px";
            criteria = { window_role = "popup"; };
          }
          {
            command = "sticky enable";
            criteria = { floating = ""; };
          }
        ];
      };
      startup = map (a: { notification = false; } // a) [
        { command = apps.browser.cmd; }
        { command = "${pkgs.kdeconnect}/libexec/kdeconnectd"; }
        {
          command =
            "${pkgs.mate.mate-polkit}/libexec/polkit-mate-authentication-agent-1";
        }
        {
          command =
            "${pkgs.keepassxc}/bin/keepassxc /home/balsoft/projects/nixos-config/misc/Passwords.kdbx";
        }
        { command = "${pkgs.termNote}/bin/noted"; }
        { command = "${pkgs.nheko}/bin/nheko"; }
        { command = "${pkgs.xorg.xrdb}/bin/xrdb -merge ~/.Xresources"; }

        { command = "${pkgs.cantata}/bin/cantata"; }

        {
          command = "swayidle -w before-sleep '${lock_fork}' lock '${lock_fork}' unlock 'pkill -9 swaylock'";
        }
      ];

      keybindings = let
        script = name: content: "exec ${pkgs.writeScript name content}";
        workspaces = (builtins.genList (x: [ (toString x) (toString x) ]) 10)
          ++ [ [ "c" "" ] [ "t" "" ] [ "m" "ﱘ" ] ];
        moveMouse = ''
          exec "sh -c 'eval `${pkgs.xdotool}/bin/xdotool \
                getactivewindow \
                getwindowgeometry --shell`; ${pkgs.xdotool}/bin/xdotool \
                mousemove \
                $((X+WIDTH/2)) $((Y+HEIGHT/2))'"'';
      in ({
        "${modifier}+q" = "kill";
        "${modifier}+Return" = "exec ${apps.term.cmd}";
        "${modifier}+e" = "exec ${apps.editor.cmd}";
        "${modifier}+l" = "layout toggle all";

        "${modifier}+Left" = "focus child; focus left; ${moveMouse}";
        "${modifier}+Right" = "focus child; focus right; ${moveMouse}";
        "${modifier}+Up" = "focus child; focus up; ${moveMouse}";
        "${modifier}+Down" = "focus child; focus down; ${moveMouse}";
        "${modifier}+Control+Left" = "focus parent; focus left; ${moveMouse}";
        "${modifier}+Control+Right" = "focus parent; focus right; ${moveMouse}";
        "${modifier}+Control+Up" = "focus parent; focus up; ${moveMouse}";
        "${modifier}+Control+Down" = "focus parent; focus down; ${moveMouse}";
        "${modifier}+Shift+Up" = "move up";
        "${modifier}+Shift+Down" = "move down";
        "${modifier}+Shift+Right" = "move right";
        "${modifier}+Shift+Left" = "move left";

        "${modifier}+a" = "focus child; focus left; ${moveMouse}";
        "${modifier}+d" = "focus child; focus right; ${moveMouse}";
        "${modifier}+w" = "focus child; focus up; ${moveMouse}";
        "${modifier}+s" = "focus child; focus down; ${moveMouse}";
        "${modifier}+Control+a" = "focus parent; focus left; ${moveMouse}";
        "${modifier}+Control+d" = "focus parent; focus right; ${moveMouse}";
        "${modifier}+Control+w" = "focus parent; focus up; ${moveMouse}";
        "${modifier}+Control+s" = "focus parent; focus down; ${moveMouse}";
        "${modifier}+Shift+w" = "move up";
        "${modifier}+Shift+s" = "move down";
        "${modifier}+Shift+d" = "move right";
        "${modifier}+Shift+a" = "move left";

        "${modifier}+f" = "fullscreen toggle; floating toggle";
        "${modifier}+r" = "mode resize";
        "${modifier}+Shift+f" = "floating toggle";

        "${modifier}+j" = "focus mode_toggle";
        "${modifier}+Escape" = "exec ${apps.monitor.cmd}";

        "${modifier}+Print" = script "screenshot"
          "${pkgs.grim}/bin/grim Pictures/$(date +'%Y-%m-%d+%H:%M:%S').png";

        "${modifier}+Control+Print" = script "screenshot-copy"
          "${pkgs.grim}/bin/grim - | ${pkgs.wl-clipboard}/bin/wl-copy";

        "--release ${modifier}+Shift+Print" = script "screenshot-area" ''
          ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" Pictures/$(date +'%Y-%m-%d+%H:%M:%S').png'';

        "--release ${modifier}+Control+Shift+Print" =
          script "screenshot-area-copy" ''
            ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - | ${pkgs.wl-clipboard}/bin/wl-copy'';

        "${modifier}+x" = "move workspace to output right";
        "${modifier}+k" = "exec '${pkgs.xorg.xkill}/bin/xkill'";
        "${modifier}+F5" = "reload";
        "${modifier}+Shift+F5" = "exit";
        "${modifier}+Shift+h" = "layout splith";
        "${modifier}+Shift+v" = "layout splitv";
        "${modifier}+h" = "split h";
        "${modifier}+v" = "split v";
        "${modifier}+F1" = "move to scratchpad";
        "${modifier}+F2" = "scratchpad show";
        "${modifier}+F11" = "output * dpms off";
        "${modifier}+F12" = "output * dpms on";
        "${modifier}+End" = "exec ${lock}";
        "${modifier}+p" = "sticky toggle";
        "${modifier}+i" =
          script "0x0" ''wl-paste | curl -F"file=@-" https://0x0.st | wl-copy'';
        "${modifier}+b" = "focus mode_toggle";
        "${modifier}+z" = script "lambda-launcher"
          "${pkgs.lambda-launcher}/bin/lambda-launcher";
        "XF86AudioPlay" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
        "XF86AudioNext" = "exec ${pkgs.playerctl}/bin/playerctl next";
        "XF86AudioPrev" = "exec ${pkgs.playerctl}/bin/playerctl previous";
        "button2" = "kill";
        "--whole-window ${modifier}+button2" = "kill";
      } // builtins.listToAttrs (builtins.map (x: {
        name = "${modifier}+${builtins.elemAt x 0}";
        value = "workspace ${builtins.elemAt x 1}";
      }) workspaces) // builtins.listToAttrs (builtins.map (x: {
        name = "${modifier}+Shift+${builtins.elemAt x 0}";
        value = "move container to workspace ${builtins.elemAt x 1}";
      }) workspaces));
      keycodebindings = {
        "122" = "exec ${pkgs.pamixer}/bin/pamixer -d 2";
        "123" = "exec ${pkgs.pamixer}/bin/pamixer -i 2";
        "121" = "exec ${pkgs.pamixer}/bin/pamixer -t";
      };
      workspaceLayout = "tabbed";
    };
    extraConfig = ''
      output * bg ${thm.bg} solid_color
      input 2:14:ETPS/2_Elantech_Touchpad {
        tap enabled
        natural_scroll enabled
        dwt enabled
      }
      input 2:14:ETPS/2_Elantech_TrackPoint pointer_accel -0.7
      default_border pixel 1
      mouse_warping container
      hide_edge_borders --i3 smart
      exec pkill swaynag
    '';
  };
}

