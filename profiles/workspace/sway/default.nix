{ pkgs, lib, config, ... }:
let
  thm = pkgs.my-lib.thmHash config.themes.colors;
  apps = config.defaultApplications;
  lock = pkgs.writeShellScript "lock"
    "swaymsg 'output * dpms off'; sudo /run/current-system/sw/bin/lock all; swaymsg 'output * dpms on'";
  htmlify = pkgs.writeShellScript "htmlify" ''
    ${pkgs.wl-clipboard}/bin/wl-paste -p | ${pkgs.pandoc}/bin/pandoc -t html | ${pkgs.wl-clipboard}/bin/wl-copy -t text/html
  '';
in {
  environment.sessionVariables = {
    _JAVA_AWT_WM_NONREPARENTING = "1";
    XDG_SESSION_TYPE = "wayland";
    QT_QPA_PLATFORM = "wayland";
    QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
  };

  programs.sway.enable = true;

  programs.sway.wrapperFeatures.gtk = true;

  programs.sway.extraPackages = lib.mkForce (with pkgs; [ swayidle xwayland ]);

  users.users.balsoft.extraGroups = [ "sway" ];

  environment.loginShellInit = lib.mkAfter ''
    [[ "$(tty)" == /dev/tty1 ]] && {
      echo fetch | gpg --card-edit --no-tty --command-fd=0
      SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
      export SSH_AUTH_SOCK
      systemctl restart --user gpg-agent.socket gpg-agent-ssh.socket
      sway
    }
  '';

  xdg.portal.wlr.enable = true;
  # xdg.portal.xdgOpenUsePortal = true;

  home-manager.users.balsoft.wayland.windowManager.sway = {
    enable = true;
    config = rec {
      assigns = {
        "" = [
          { class = "Chromium"; }
          { app_id = "firefox"; }
          { class = "Firefox"; }
        ];
        "󰍩" =
          [ { app_id = "nheko"; } { title = "Slack.*"; } { title = "aerc"; } ];
      };
      fonts = {
        names = [ config.themes.fonts.main.family ];
        style = "Regular";
        size = 9.0;
      };

      colors = rec {
        background = thm.base00;
        unfocused = {
          text = thm.base03;
          border = thm.base01;
          background = thm.base00;
          childBorder = thm.base01;
          indicator = thm.base07;
        };
        focusedInactive = unfocused;
        urgent = unfocused // {
          text = thm.base05;
          border = thm.base09;
          childBorder = thm.base09;
        };
        focused = unfocused // {
          childBorder = thm.base03;
          border = thm.base03;
          background = thm.base01;
          text = thm.base05;
        };
      };
      gaps = {
        inner = 6;
        smartGaps = true;
        smartBorders = "on";
      };
      focus.followMouse = false;
      focus.wrapping = "force";
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
      startup = (map (command: { inherit command; }) config.startupApplications)
        ++ [{
          command =
            "dbus-update-activation-environment --systemd WAYLAND_DISPLAY GDK_BACKEND";
        }];

      bindkeysToCode = true;

      modes = {

        normal = let
          script = name: content: "exec ${pkgs.writeScript name content}";
          workspaces = (builtins.genList (x: [ (toString x) (toString x) ]) 10)
            ++ [ [ "c" "" ] [ "t" "󰍩" ] [ "m" "ﱘ" ] ];
        in ({
          "Escape" = "mode default";
          "Return" = "mode default";
          "i" = "mode default";
          "r" = "mode resize";

          "q" = "kill";
          "Shift+q" =
            "move container to workspace temp; [workspace=__focused__] kill; workspace temp; move container to workspace temp; workspace temp";
          "o" = "layout toggle all";

          "Left" = "focus child; focus left";
          "Right" = "focus child; focus right";
          "Up" = "focus child; focus up";
          "Down" = "focus child; focus down";
          "Control+Left" = "focus parent; focus left";
          "Control+Right" = "focus parent; focus right";
          "Control+Up" = "focus parent; focus up";
          "Control+Down" = "focus parent; focus down";
          "Shift+Up" = "move up";
          "Shift+Down" = "move down";
          "Shift+Right" = "move right";
          "Shift+Left" = "move left";

          "Comma" = "workspace prev";
          "Period" = "workspace next";

          "h" = "focus child; focus left";
          "l" = "focus child; focus right";
          "k" = "focus child; focus up";
          "j" = "focus child; focus down";
          "Control+h" = "focus parent; focus left";
          "Control+l" = "focus parent; focus right";
          "Control+k" = "focus parent; focus up";
          "Control+j" = "focus parent; focus down";
          "Shift+k" = "move up";
          "Shift+j" = "move down";
          "Shift+l" = "move right";
          "Shift+h" = "move left";
          "u" = "focus parent";

          "f" = "fullscreen toggle; floating toggle";
          "Shift+f" = "floating toggle";

          "Shift+Escape" =
            ''exec ${apps.monitor.cmd}; [app_id="gnome-system-monitor"] focus'';
          "F1" = ''
            exec ${pkgs.pavucontrol}/bin/pavucontrol; [app_id="pavucontrol"] focus'';
          "Shift+F1" = ''
            exec ${pkgs.qpwgraph}/bin/qpwgraph; [app_id="org.freedesktop.ryuukyu.Helvum"] focus'';
          "F3" = "exec ${pkgs.alsa-utils}/bin/amixer set Capture cap";
          "Shift+F3" = "exec ${pkgs.alsa-utils}/bin/amixer set Capture nocap";
          "F5" = "reload";
          "Shift+F5" = "exit";
          "z" = "exec ${pkgs.mako}/bin/makoctl dismiss";
          "Shift+z" = "exec ${pkgs.mako}/bin/makoctl restore";
          "Control+z" = "exec ${pkgs.mako}/bin/makoctl dismiss -a";
          "F9" = ''
            exec ${pkgs.libnotify}/bin/notify-send "Do not disturb: on"; exec ${pkgs.mako}/bin/makoctl set-mode do-not-disturb; bar mode invisible'';
          "Shift+F9" = ''
            exec ${pkgs.libnotify}/bin/notify-send "Do not disturb: off"; exec ${pkgs.mako}/bin/makoctl set-mode default; bar mode hide'';
          "F11" = "exec ${pkgs.systemd}/bin/systemctl suspend";
          "Shift+F11" = "output * dpms off";
          "F12" = "output * dpms on";
          "End" = "exec ${lock}";

          "Slash" = "exec ${pkgs.copyq}/bin/copyq menu";
          "Shift+Slash" = "exec ${htmlify}";

          "Print" = script "screenshot"
            "${pkgs.grim}/bin/grim Pictures/$(date +'%Y-%m-%d+%H:%M:%S').png";

          "Control+Print" = script "screenshot-copy"
            "${pkgs.grim}/bin/grim - | ${pkgs.wl-clipboard}/bin/wl-copy";

          "--release Shift+Print" = script "screenshot-area" ''
            ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" Pictures/$(date +'%Y-%m-%d+%H:%M:%S').png'';

          "--release Control+Shift+Print" = script "screenshot-area-copy" ''
            ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - | ${pkgs.wl-clipboard}/bin/wl-copy'';

          "--release Insert" = script "screenshot-ocr" ''
            ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - | ${pkgs.tesseract5}/bin/tesseract -l eng - - | ${pkgs.wl-clipboard}/bin/wl-copy'';

          "x" = "focus output right";
          "Shift+x" = "move workspace to output right";
          "quotedbl" = "layout splith";
          "apostrophe" = "layout splitv";
          "minus" = "move to scratchpad";
          "underscore" = "scratchpad show";
          "p" = "sticky toggle";
          "b" = "focus mode_toggle";
          "Space" = script "lambda-launcher"
            "${pkgs.lambda-launcher}/bin/lambda-launcher";
          "XF86AudioPlay" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
          "XF86AudioPause" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
          "XF86AudioNext" = "exec ${pkgs.playerctl}/bin/playerctl next";
          "XF86AudioPrev" = "exec ${pkgs.playerctl}/bin/playerctl previous";
          "XF86AudioLowerVolume" = "exec ${pkgs.pamixer}/bin/pamixer -d 2";
          "XF86AudioRaiseVolume" = "exec ${pkgs.pamixer}/bin/pamixer -i 2";
          "XF86AudioMute" = "exec ${pkgs.pamixer}/bin/pamixer -t";
          "button2" = "kill";
          "Next" = "input * xkb_layout 'us,ru'";
          "Prior" = "input * xkb_layout 'ge'";
        } // builtins.listToAttrs (builtins.map (x: {
          name = "${builtins.elemAt x 0}";
          value = "workspace ${builtins.elemAt x 1}";
        }) workspaces) // builtins.listToAttrs (builtins.map (x: {
          name = "Shift+${builtins.elemAt x 0}";
          value = "move container to workspace ${builtins.elemAt x 1}";
        }) workspaces));

        resize = {
          Down = "resize grow height 50 px";
          Escape = "mode default";
          Left = "resize shrink width 50 px";
          Return = "mode default";
          Right = "resize grow width 50 px";
          Up = "resize shrink height 50 px";
          h = "resize shrink width 50 px";
          j = "resize grow height 50 px";
          k = "resize shrink height 50 px";
          l = "resize grow width 50 px";
        };
      };

      keybindings = lib.mapAttrs' (name:
        let
          s = lib.splitString " " name;
          flags = lib.init s;
        in lib.nameValuePair "${builtins.concatStringsSep " " flags}${
          lib.optionalString (builtins.length flags != 0) " "
        }${modifier}+${lib.last s}") modes.normal // {
          "${modifier}+Escape" = "mode normal";
          "${modifier}+Return" = "exec ${apps.term.cmd}";
          "XF86AudioPlay" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
          "XF86AudioPause" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
          "XF86AudioNext" = "exec ${pkgs.playerctl}/bin/playerctl next";
          "XF86AudioPrev" = "exec ${pkgs.playerctl}/bin/playerctl previous";
          "XF86AudioLowerVolume" = "exec ${pkgs.pamixer}/bin/pamixer -d 2";
          "XF86AudioRaiseVolume" = "exec ${pkgs.pamixer}/bin/pamixer -i 2";
          "XF86AudioMute" = "exec ${pkgs.pamixer}/bin/pamixer -t";
          "button2" = "kill";
          "--whole-window ${modifier}+button2" = "kill";
        };

      keycodebindings = { };
      workspaceLayout = "tabbed";
      workspaceAutoBackAndForth = true;
      input = {
        "2:14:ETPS/2_Elantech_Touchpad" = {
          tap = "enabled";
          natural_scroll = "enabled";
          dwt = "enabled";
        };
        "2:14:ETPS/2_Elantech_TrackPoint" = { pointer_accel = "-0.7"; };
        "2:10:TPPS/2_IBM_TrackPoint" = {
          pointer_accel = "0.4";
          accel_profile = "adaptive";
        };
      };
      output = {
        "*".bg = "${thm.base00} solid_color";
      } // lib.optionalAttrs (config.device == "AMD-Workstation") {
        DP-1 = {
          transform = "270";
          position = "0 0";
        };
        HDMI-A-1 = { position = "1440 1000"; };
      } // lib.optionalAttrs (config.device == "X2100-Laptop") {
        eDP-1.scale = "1.9";
      } // lib.optionalAttrs (config.device == "T490s-Laptop") {
        DP-2.position = "0 0";
      };
    };
    wrapperFeatures = { gtk = true; };
    extraConfig = ''
      default_border pixel 1
      mouse_warping container
      hide_edge_borders --i3 smart
      exec pkill swaynag
    '';
  };
}
