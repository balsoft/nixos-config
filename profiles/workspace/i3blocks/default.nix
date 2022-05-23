{ pkgs, config, lib, ... }:
with lib; with pkgs.my-lib;
let scripts = import ./scripts pkgs config;
in {
  secrets.wage = {
    owner = "balsoft:users";
    services = [ ];
  };

  secrets.email = {
    owner = "balsoft:users";
    services = [ ];
    encrypted = "${config.environment.sessionVariables.PASSWORD_STORE_DIR}/email/balsoft@balsoft.ru.gpg";
  };

  home-manager.users.balsoft = {
    wayland.windowManager.sway.config.bars = [{
      id = "default";
      trayOutput = "none";
      command = "true";
      colors = let
        thm = pkgs.my-lib.thmHash config.themes.colors;
        default = {
          background = thm.base00;
          border = thm.base00;
        };
      in {
        background = thm.base00;
        statusline = thm.base05;
        separator = thm.base02;
        focusedWorkspace = default // { text = thm.base08; };
        activeWorkspace = default // { text = thm.base0B; };
        inactiveWorkspace = default // { text = thm.base05; };
        urgentWorkspace = default // { text = thm.base09; };
        bindingMode = default // { text = thm.base0A; };
      };
      statusCommand = "${pkgs.i3blocks}/bin/i3blocks";
      fonts = {
        names = [ "${config.themes.fonts.main.family}" "Material Icons" "Roboto Mono" ];
        style = "Regular";
        size = 12.0;
      };
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
    '' + pkgs.my-lib.genIniOrdered ([ (scr "email") ]
      ++ [ (scrint "weather" 600) (scr "emacs") (scr "nixos") ]
      ++ [ (scrint "music" 3) (scrint "sound" 1) ]
      ++ [
        (scrint "cpu" 5)
        (scrint "freq" 10)
        (scr "temperature")
        (scrint "free" 10)
        (scr "battery")
      ] ++ optionals config.deviceSpecific.isLaptop [
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
      Unit = {
        Description = "Start default bar";
        X-RestartIfChanged = true;
      };

      Install.WantedBy = [ "sway-session.target" ];

      Service = { ExecStart = "${pkgs.sway}/bin/swaybar -b default"; };
    };
  };
}
