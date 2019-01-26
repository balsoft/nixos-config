{pkgs, config, ...}:
let thm = config.themes.colors;
    apps = config.defaultApplications;
    customPackages = pkgs.callPackage ../../packages {};
in
{
  home-manager.users.balsoft.xsession.windowManager.i3 = {
    enable = true;
    package = pkgs.i3-gaps;
    config = rec {
      assigns = {
        "" = [{ class = "Chromium"; } { class = "Firefox"; } ];
        "" = [{ class = "^Telegram"; } { class = "^VK"; } { class = "^trojita"; } ];
      };
      bars = [];
      fonts = [ "RobotoMono 9" ];
      
      colors = rec{
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
      focus.mouseWarping = true;
      modifier = "Mod4";
      window = {
        border = 1;
        titlebar = false;
        hideEdgeBorders = "smart";
        commands = [ 
          {
            command = "focus";
            criteria = { urgent = "latest"; };
          } 
          {
            command = "border pixel 1px";
            criteria = { window_role = "popup"; };
          }
        ];
      };
      startup = map (a: { notification = false; } // a) [
        { command = "${pkgs.albert}/bin/albert"; always = true; }
        { command = "${pkgs.tdesktop}/bin/telegram-desktop"; }
        { command = apps.browser.cmd; }
        { command = "${customPackages.vk}/bin/vk"; }
        { command = "emacs  --daemon"; }
        { command = "${pkgs.kdeconnect}/lib/libexec/kdeconnectd"; }
        { command = "${pkgs.polkit-kde-agent}/lib/libexec/polkit-kde-authentication-agent-1"; }
        { command = "${pkgs.keepassxc}/bin/keepassxc /home/balsoft/projects/nixos-config/misc/Passwords.kdbx"; }
        { command = "balooctl start"; }
        { command = "${pkgs.autorandr}/bin/autorandr --force horizontal"; always = true; }
        { command = "${pkgs.trojita}/bin/trojita"; } 
        { command = apps.term.cmd; workspace = "0"; }
        { command = "${pkgs.rclone}/bin/rclone mount google:/ '/home/balsoft/Google Drive' --verbose --daemon"; }
        { command = "${pkgs.hsetroot}/bin/hsetroot -solid '${thm.bg}'"; always = true; }
        { command = ''${pkgs.i3}/bin/i3-msg 'workspace ""; layout tabbed;' ''; always = true; }
        { command = "${pkgs.termNote}/bin/noted"; }
      ];
      keybindings = let moveMouse = ''"sh -c 'eval `${
        pkgs.xdotool
      }/bin/xdotool \
      getactivewindow \
      getwindowgeometry --shell`; ${
        pkgs.xdotool
      }/bin/xdotool \
      mousemove \
      $((X+WIDTH/2)) $((Y+HEIGHT/2))'"''; in
      ({
        "${modifier}+q" = "kill";
        "${modifier}+Return" = "exec ${apps.term.cmd}";
        "${modifier}+e" = "exec ${apps.editor.cmd} -c -n";
        "${modifier}+l" = "layout toggle";
        "${modifier}+Left" = "focus child; focus left; exec ${moveMouse}";
        "${modifier}+Right" = "focus child; focus right; exec ${moveMouse}";
        "${modifier}+Up" = "focus child; focus up; exec ${moveMouse}";
        "${modifier}+Down" = "focus child; focus down; exec ${moveMouse}";
        "${modifier}+Control+Left" = "focus parent; focus left; exec ${moveMouse}";
        "${modifier}+Control+Right" = "focus parent; focus right; exec ${moveMouse}";
        "${modifier}+Control+Up" = "focus parent; focus up; exec ${moveMouse}";
        "${modifier}+Control+Down" = "focus parent; focus down; exec ${moveMouse}";
        "${modifier}+Shift+Up" = "move up";
        "${modifier}+Shift+Down" = "move down";
        "${modifier}+Shift+Right" = "move right";
        "${modifier}+Shift+Left" = "move left";
        "${modifier}+f" = "fullscreen toggle";
        "${modifier}+r" = "mode resize";
        "${modifier}+Shift+f" = "floating toggle";
        "${modifier}+d" = "exec ${pkgs.dolphin}/bin/dolphin";
        "${modifier}+Escape" = "exec ${pkgs.ksysguard}/bin/ksysguard";
        "${modifier}+Print" = "exec scrot -e 'mv $f ~/Pictures && notify-send \"Screenshot saved as ~/Pictures/$f\"'";
        "${modifier}+Control+Print" = "exec scrot -e 'xclip -selection clipboard -t image/png -i $f && notify-send \"Screenshot copied to clipboard\" && rm $f'";
        "--release ${modifier}+Shift+Print" = "exec scrot -s -e 'mv $f ~/Pictures && notify-send \"Screenshot saved as ~/Pictures/$f\"'";
        "--release ${modifier}+Control+Shift+Print" = "exec scrot -s -e 'xclip -selection clipboard -t image/png -i $f && notify-send \"Screenshot copied to clipboard\" && rm $f'";
        "${modifier}+x" = "move workspace to output right"; 
        "${modifier}+z" = "exec ${pkgs.i3-easyfocus}/bin/i3-easyfocus";
        "${modifier}+c" = "workspace ";
        "${modifier}+Shift+c" = "move container to workspace ";
        "${modifier}+t" = "workspace ";
        "${modifier}+Shift+t" = "move container to workspace ";
        "${modifier}+k" = "exec '${pkgs.xorg.xkill}/bin/xkill'";
        "${modifier}+F5" = "restart";
        "${modifier}+Shift+F5" = "exit";
        "${modifier}+h" = "layout splith";
        "${modifier}+v" = "layout splitv";
        "${modifier}+Minus" = "move to scratchpad";
        "${modifier}+Equals" = "scratchpad show";
      } // builtins.listToAttrs (
        builtins.genList (x: {name = "${modifier}+${toString x}"; value = "workspace ${toString x}";}) 10
      ) // builtins.listToAttrs (
        builtins.genList (x: {name = "${modifier}+Shift+${toString x}"; value = "move container to workspace ${toString x}";}) 10
      ));
      keycodebindings = {
        "122" = "exec ${pkgs.pamixer}/bin/pamixer -d 5";
        "123" = "exec ${pkgs.pamixer}/bin/pamixer -i 5";
        "121" = "exec ${pkgs.pamixer}/bin/pamixer -t";
        "164" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
        "163" = "exec ${pkgs.playerctl}/bin/playerctl next";
        "165" = "exec ${pkgs.playerctl}/bin/playerctl previous";
      };
    };
  };
}
