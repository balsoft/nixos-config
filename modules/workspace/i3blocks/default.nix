{ pkgs, config, lib, ... }:
with import ../../../support.nix { inherit pkgs config lib; };
with lib;
let scripts = import ./scripts pkgs config;
in {
  home-manager.users.balsoft = {
    wayland.windowManager.sway.config.bars = [{
      id = "default";
      trayOutput = "primary";
      command = "true";
      colors = let
        thm = config.themes.colors;
        default = {
          background = thm.bg;
          border = thm.bg;
        };
      in {
        background = thm.bg;
        statusline = thm.fg;
        separator = thm.alt;
        focusedWorkspace = default // { text = thm.red; };
        activeWorkspace = default // { text = thm.green; };
        inactiveWorkspace = default // { text = thm.fg; };
        urgentWorkspace = default // { text = thm.orange; };
        bindingMode = default // { text = thm.yellow; };
      };
      statusCommand = "${pkgs.i3blocks}/bin/i3blocks";
      fonts = [ "IBM Plex Mono 11" "Material Icons 11" "Roboto Mono 11" ];
      mode = "hide";
      position = "bottom";
      workspaceNumbers = false;
    }];
    xdg.configFile."i3blocks/config".text = let
      scr = x: {
        name = x;
        command = scripts.${x};
      };
      scrint = x: interval: (scr x) // { inherit interval; };
    in ''
      interval=60
      markup=pango
    '' + genIniOrdered (optional (!isNull config.secrets.mail) (scr "email")
      ++ [ (scrint "weather" 600) (scr "calendar") (scr "emacs") (scr "nixos") ]
      ++ optional (!isNull config.secrets.wage) (scrint "youtrack-wage" 3600)
      ++ [ (scrint "music" 3) (scrint "sound" 1) ] ++ [
        (scrint "cpu" 5)
        (scrint "freq" 10)
        (scr "temperature")
        (scrint "free" 10)
      ] ++ optionals config.deviceSpecific.isLaptop [
        (scr "battery")
        (scrint "brightness" 5)
      ] ++ optional (config.deviceSpecific.devInfo ? bigScreen)
      (scrint "network" 1) ++ [
        (scrint "bluetooth" 3)
        (scrint "connections" 3)
        (scr "df")
        (scr "date")
        (scrint "time" 1)
      ]);
    systemd.user.services.swaybar = {
      Unit = { Description = "Start default bar"; };

      Install.WantedBy = [ "sway-session.target" ];

      Service = { ExecStart = "${pkgs.sway}/bin/swaybar -b default"; };
    };
  };
}
