{ pkgs, lib, config, ... }:
let
  thm = pkgs.my-lib.thmHash config.themes.colors;
  apps = config.defaultApplications;
  lock_fork =
    pkgs.writeShellScript "lock_fork" "sudo /run/current-system/sw/bin/lock &";
  lock = pkgs.writeShellScript "lock"
    "swaymsg 'output * dpms off'; sudo /run/current-system/sw/bin/lock; swaymsg 'output * dpms on'";
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
      pass unlock
      sway
    }
  '';

  home-manager.users.balsoft.wayland.windowManager.sway = {
    enable = true;
    config = rec {
      assigns = {
        "" = [
          { class = "Chromium"; }
          { app_id = "firefox"; }
          { class = "Firefox"; }
        ];
        "" =
          [ { app_id = "geary"; } { title = "nheko"; } { title = "Slack.*"; } ];
      };
      fonts = {
        names = [ config.themes.fonts.main.family ];
        style = "Regular";
        size = 9.0;
      };

      colors = rec {
        background = thm.base00;
        unfocused = {
          text = thm.base02;
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
      focus.forceWrapping = true;
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
        ++ [
          {
            command =
              "swayidle -w before-sleep '${lock_fork}' lock '${lock_fork}' unlock 'pkill -9 swaylock'";
          }
          {
            command =
              "dbus-update-activation-environment --systemd WAYLAND_DISPLAY GDK_BACKEND";
          }
        ];

      keybindings = let
        script = name: content: "exec ${pkgs.writeScript name content}";
        workspaces = (builtins.genList (x: [ (toString x) (toString x) ]) 10)
          ++ [ [ "c" "" ] [ "t" "" ] [ "m" "ﱘ" ] ];
      in ({
        "${modifier}+q" = "kill";
        "${modifier}+Shift+q" =
          "move container to workspace temp; [workspace=__focused__] kill; workspace temp; move container to workspace temp; workspace temp";
        "${modifier}+Return" = "exec ${apps.term.cmd}";
        "${modifier}+e" = "exec ${apps.editor.cmd}";
        "${modifier}+o" = "layout toggle all";

        "${modifier}+Left" = "focus child; focus left";
        "${modifier}+Right" = "focus child; focus right";
        "${modifier}+Up" = "focus child; focus up";
        "${modifier}+Down" = "focus child; focus down";
        "${modifier}+Control+Left" = "focus parent; focus left";
        "${modifier}+Control+Right" = "focus parent; focus right";
        "${modifier}+Control+Up" = "focus parent; focus up";
        "${modifier}+Control+Down" = "focus parent; focus down";
        "${modifier}+Shift+Up" = "move up";
        "${modifier}+Shift+Down" = "move down";
        "${modifier}+Shift+Right" = "move right";
        "${modifier}+Shift+Left" = "move left";

        "${modifier}+Comma" = "workspace prev";
        "${modifier}+Period" = "workspace next";

        "${modifier}+a" = "focus child; focus left";
        "${modifier}+d" = "focus child; focus right";
        "${modifier}+w" = "focus child; focus up";
        "${modifier}+s" = "focus child; focus down";
        "${modifier}+Control+a" = "focus parent; focus left";
        "${modifier}+Control+d" = "focus parent; focus right";
        "${modifier}+Control+w" = "focus parent; focus up";
        "${modifier}+Control+s" = "focus parent; focus down";
        "${modifier}+Shift+w" = "move up";
        "${modifier}+Shift+s" = "move down";
        "${modifier}+Shift+d" = "move right";
        "${modifier}+Shift+a" = "move left";

        "${modifier}+f" = "fullscreen toggle; floating toggle";
        "${modifier}+r" = "mode resize";
        "${modifier}+Shift+f" = "floating toggle";

        "${modifier}+Escape" =
          ''exec ${apps.monitor.cmd}; [app_id="gnome-system-monitor"] focus'';
        "${modifier}+F1" = ''
          exec ${pkgs.pavucontrol}/bin/pavucontrol; [app_id="pavucontrol"] focus'';
        "${modifier}+Shift+F1" = ''
          exec ${
            pkgs.writeShellScript "helvum"
            "GTK_THEME=adwaita:dark ${pkgs.helvum}/bin/helvum"
          }; [app_id="org.freedesktop.ryuukyu.Helvum"] focus'';
        "${modifier}+F3" = "exec ${pkgs.alsa-utils}/bin/amixer set Capture cap";
        "${modifier}+Shift+F3" = "exec ${pkgs.alsa-utils}/bin/amixer set Capture nocap";
        "${modifier}+F5" = "reload";
        "${modifier}+Shift+F5" = "exit";
        "${modifier}+F9" = "exec systemctl --user restart mako.service";
        "${modifier}+Shift+F9" = "exec systemctl --user stop mako.service";
        "${modifier}+F11" = "output * dpms off";
        "${modifier}+F12" = "output * dpms on";
        "${modifier}+End" = "exec ${lock}";

        "${modifier}+j" = "exec ${pkgs.playerctl}/bin/playerctl previous";
        "${modifier}+k" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
        "${modifier}+l" = "exec ${pkgs.playerctl}/bin/playerctl next";

        "${modifier}+Print" = script "screenshot"
          "${pkgs.grim}/bin/grim Pictures/$(date +'%Y-%m-%d+%H:%M:%S').png";

        "${modifier}+Control+Print" = script "screenshot-copy"
          "${pkgs.grim}/bin/grim - | ${pkgs.wl-clipboard}/bin/wl-copy";

        "--release ${modifier}+Shift+Print" = script "screenshot-area" ''
          ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" Pictures/$(date +'%Y-%m-%d+%H:%M:%S').png'';

        "--release ${modifier}+Control+Shift+Print" =
          script "screenshot-area-copy" ''
            ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - | ${pkgs.wl-clipboard}/bin/wl-copy'';

        "${modifier}+x" = "focus output right";
        "${modifier}+Shift+x" = "move workspace to output right";
        "${modifier}+Shift+h" = "layout splith";
        "${modifier}+Shift+v" = "layout splitv";
        "${modifier}+h" = "split h";
        "${modifier}+v" = "split v";
        "${modifier}+i" = "move to scratchpad";
        "${modifier}+Shift+i" = "scratchpad show";
        "${modifier}+p" = "sticky toggle";
        "${modifier}+b" = "focus mode_toggle";
        "${modifier}+Space" = script "lambda-launcher"
          "${pkgs.lambda-launcher}/bin/lambda-launcher";
        "XF86AudioPlay" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
        "XF86AudioNext" = "exec ${pkgs.playerctl}/bin/playerctl next";
        "XF86AudioPrev" = "exec ${pkgs.playerctl}/bin/playerctl previous";
        "XF86AudioLowerVolume" = "exec ${pkgs.pamixer}/bin/pamixer -d 2";
        "XF86AudioRaiseVolume" = "exec ${pkgs.pamixer}/bin/pamixer -i 2";
        "XF86AudioMute" = "exec ${pkgs.pamixer}/bin/pamixer -t";
        "${modifier}+XF86AudioLowerVolume" =
          "exec ${pkgs.pamixer}/bin/pamixer -d 1";
        "${modifier}+XF86AudioRaiseVolume" =
          "exec ${pkgs.pamixer}/bin/pamixer -i 1";
        "button2" = "kill";
        "--whole-window ${modifier}+button2" = "kill";
      } // builtins.listToAttrs (builtins.map (x: {
        name = "${modifier}+${builtins.elemAt x 0}";
        value = "workspace ${builtins.elemAt x 1}";
      }) workspaces) // builtins.listToAttrs (builtins.map (x: {
        name = "${modifier}+Shift+${builtins.elemAt x 0}";
        value = "move container to workspace ${builtins.elemAt x 1}";
      }) workspaces));
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
        DP-1.position = "0 400";
        HDMI-A-1 = {
          transform = "90";
          position = "2560 0";
        };
      } // lib.optionalAttrs (config.device == "X2100-Laptop") {
        "Unknown 0x0000 0x00000000".scale = "2";
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
