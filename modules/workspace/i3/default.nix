{ pkgs, lib, config, ... }:
let
  thm = config.themes.colors;
  apps = config.defaultApplications;
in {
  environment.sessionVariables._JAVA_AWT_WM_NONREPARENTING = "1";  

  home-manager.users.balsoft.xsession.windowManager.i3 = {
    enable = true;
    package = pkgs.i3-gaps;
    config = rec {
      assigns = {
        "" = [
          { class = "Chromium"; }
          { app_id = "firefox"; }
          { app_id = "keepassxc"; }
        ];
        "" = [
          { class = "telegram"; }
          { class = "^VK"; }
          { app_id = "net.flaska.trojita"; }
          { title = "weechat"; }
          { title = "nheko"; }
        ];
        "ﱘ" = [{ class = "cantata"; }];
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
            command = "floating disable";
            criteria = { class = "pavucontrol-qt"; };
          }
        ];
      };
      startup = map (a: { notification = false; } // a) [
        { command = apps.browser.cmd; }
        { command = "${pkgs.kdeconnect}/lib/libexec/kdeconnectd"; }
        {
          command =
            "${pkgs.polkit-kde-agent}/lib/libexec/polkit-kde-authentication-agent-1";
        }
        {
          command =
            "${pkgs.keepassxc}/bin/keepassxc /home/balsoft/projects/nixos-config/misc/Passwords.kdbx";
        }
        { command = "balooctl start"; }
        { command = "${pkgs.trojita}/bin/trojita"; }
        { command = "${pkgs.termNote}/bin/noted"; }
        { command = "${pkgs.nheko}/bin/nheko"; }
        { command = "${pkgs.xorg.xrdb}/bin/xrdb -merge ~/.Xresources"; }

        (lib.mkIf (config.device == "AMD-Workstation") {
          command = ''
            NIX_ANDROID_EMULATOR_FLAGS="-no-audio -no-window" ${
              with import <nixpkgs> {
                config.android_sdk.accept_license = true;
              };
              androidenv.emulateApp {
                name = "WhatsApp";
                app = fetchurl {
                  url =
                    "https://www.cdn.whatsapp.net/android/2.19.214/WhatsApp.apk";
                  sha256 =
                    "1yc8zhlx86gb2ixizxgm2mp6dz8c47xa7w7jjaisb2v4ywmlmdmh";
                };
                platformVersion = "18";
                useGoogleAPIs = true;
                enableGPU = true;
                abiVersion = "x86";

                package = "com.whatsapp";
                activity = ".HomeActivity";

                avdHomeDir = "$HOME/.whatsapp";
              }
            }/bin/run-test-emulator
          '';
        })
      ];

      keybindings = let
        script = name: content: "exec ${pkgs.writeScript name content}";
        moveMouse = ''
          "sh -c 'eval `${pkgs.xdotool}/bin/xdotool \
                getactivewindow \
                getwindowgeometry --shell`; ${pkgs.xdotool}/bin/xdotool \
                mousemove \
                $((X+WIDTH/2)) $((Y+HEIGHT/2))'"'';
      in ({
        "${modifier}+q" = "kill";
        "${modifier}+Return" = "exec ${apps.term.cmd}";
        "${modifier}+e" = "exec ${apps.editor.cmd} -c -n";
        "${modifier}+l" = "layout toggle all";
        "${modifier}+Left" = "focus child; focus left; exec ${moveMouse}";
        "${modifier}+Right" = "focus child; focus right; exec ${moveMouse}";
        "${modifier}+Up" = "focus child; focus up; exec ${moveMouse}";
        "${modifier}+Down" = "focus child; focus down; exec ${moveMouse}";
        "${modifier}+Control+Left" =
          "focus parent; focus left; exec ${moveMouse}";
        "${modifier}+Control+Right" =
          "focus parent; focus right; exec ${moveMouse}";
        "${modifier}+Control+Up" = "focus parent; focus up; exec ${moveMouse}";
        "${modifier}+Control+Down" =
          "focus parent; focus down; exec ${moveMouse}";
        "${modifier}+Shift+Up" = "move up";
        "${modifier}+Shift+Down" = "move down";
        "${modifier}+Shift+Right" = "move right";
        "${modifier}+Shift+Left" = "move left";
        "${modifier}+f" = "fullscreen toggle";
        "${modifier}+r" = "mode resize";
        "${modifier}+Shift+f" = "floating toggle";
        "${modifier}+d" = "exec ${apps.fm.cmd}";
        "${modifier}+Escape" = "exec ${apps.monitor.cmd}";

        "${modifier}+Print" = script "screenshot"
          "${pkgs.grim}/bin/grim Pictures/$(date +'%Y-%m-%d+%H:%M:%S').png";

        "${modifier}+Control+Print" = script "screenshot-copy"
          ''${pkgs.grim}/bin/grim - | curl -F"file=@-" https://0x0.st | ${pkgs.wl-clipboard}/bin/wl-copy'';

        "--release ${modifier}+Shift+Print" = script "screenshot-area"
          ''${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" Pictures/$(date +'%Y-%m-%d+%H:%M:%S').png'';

        "--release ${modifier}+Control+Shift+Print" =
          script "screenshot-area-copy"
          ''${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - | curl -F"file=@-" https://0x0.st | ${pkgs.wl-clipboard}/bin/wl-copy'';

        "${modifier}+x" = "move workspace to output right";
        "${modifier}+c" = "workspace ";
        "${modifier}+Shift+c" = "move container to workspace ";
        "${modifier}+t" = "workspace ";
        "${modifier}+Shift+t" = "move container to workspace ";
        "${modifier}+m" = "workspace ﱘ";
        "${modifier}+Shift+m" = "move container to workspace ﱘ";
        "${modifier}+k" = "exec '${pkgs.xorg.xkill}/bin/xkill'";
        "${modifier}+F5" = "restart";
        "${modifier}+Shift+F5" = "exit";
        "${modifier}+Shift+h" = "layout splith";
        "${modifier}+Shift+v" = "layout splitv";
        "${modifier}+h" = "split h";
        "${modifier}+v" = "split v";
        "${modifier}+F1" = "move to scratchpad";
        "${modifier}+F2" = "scratchpad show";
        "${modifier}+i" = script "ix" ''wl-paste | curl -F "f:1=<-" ix.io | wl-copy'';
        "${modifier}+z" =
          script "lambda-launcher" "GDK_BACKEND=x11 ${pkgs.lambda-launcher}/bin/lambda-launcher";
        "XF86AudioPlay" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
        "XF86AudioNext" = "exec ${pkgs.playerctl}/bin/playerctl next";
        "XF86AudioPrev" = "exec ${pkgs.playerctl}/bin/playerctl previous";
        "button2" = "kill";
        "--whole-window ${modifier}+button2" = "kill";
      } // builtins.listToAttrs (builtins.genList (x: {
        name = "${modifier}+${toString x}";
        value = "workspace ${toString x}";
      }) 10) // builtins.listToAttrs (builtins.genList (x: {
        name = "${modifier}+Shift+${toString x}";
        value = "move container to workspace ${toString x}";
      }) 10));
      keycodebindings = {
        "122" = "exec ${pkgs.pamixer}/bin/pamixer -d 1";
        "123" = "exec ${pkgs.pamixer}/bin/pamixer -i 1";
        "121" = "exec ${pkgs.pamixer}/bin/pamixer -t";
      };
      workspaceLayout = "tabbed";
    };
    extraConfig = ''
      output * bg ${thm.bg} solid_color
      input 2:7:SynPS/2_Synaptics_TouchPad {
      tap enabled
      natural_scroll enabled
      }
      input 2:18:FocalTechPS/2_FocalTech_Touchpad {
      tap enabled
      natural_scroll enabled
      }
      default_border pixel 1
      mouse_warping container
      swaynag_command -
      hide_edge_borders --i3 smart
    '';
  };
}

