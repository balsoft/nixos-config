{ pkgs, config, lib, ... }:
with import ../../../support.nix { inherit pkgs config lib; };
with lib;
let scripts = import ./scripts pkgs config;
in {
  home-manager.users.balsoft.xsession.windowManager.i3.extraConfig = ''
    bar {
      id top
      font pango:Material Icons 11, Roboto Mono 11
      mode dock
      hidden_state hide
      position top
      status_command ${pkgs.i3blocks}/bin/i3blocks
      workspace_buttons yes
      strip_workspace_numbers no
      tray_output none
      colors {
        background ${config.themes.colors.bg}
        statusline ${config.themes.colors.fg}
        separator ${config.themes.colors.alt}
        focused_workspace ${config.themes.colors.bg} ${config.themes.colors.bg} ${config.themes.colors.blue} 
        active_workspace ${config.themes.colors.bg} ${config.themes.colors.bg} ${config.themes.colors.green} 
        inactive_workspace ${config.themes.colors.bg} ${config.themes.colors.bg} ${config.themes.colors.fg} 
        urgent_workspace ${config.themes.colors.bg} ${config.themes.colors.bg} ${config.themes.colors.orange} 
        binding_mode ${config.themes.colors.bg} ${config.themes.colors.bg} ${config.themes.colors.yellow} 
      }
    }
  '';

  home-manager.users.balsoft.xdg.configFile."i3blocks/config".text = let
    scr = x: {
      name = x;
      command = scripts.${x};
    };
    scrint = x: interval: (scr x) // { inherit interval; };
  in ''
    interval=60
    markup=pango
  ''
  + genIniOrdered (
    optional (config.secrets ? gmail) (scr "email")
  ++ [
    (scrint "weather" 600)
    (scr "calendar")
    (scr "emacs")
    (scrint "music" 1)
    (scrint "sound" 1)
  ]
  ++ optionals config.deviceSpecific.isLaptop [
    (scr "battery")
    (scrint "brightness" 1)
  ]
  ++ [
    (scr "connections")
    (scrint "network" 1)
    (scrint "cpu" 5)
    (scr "freq")
    (scr "temperature")
    (scr "free")
    (scr "df")
    (scr "date")
    (scr "time")
  ]);
}
