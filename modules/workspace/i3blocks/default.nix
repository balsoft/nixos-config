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

  home-manager.users.balsoft.xdg.configFile."i3blocks/config".text = ''
    interval=60
    markup=pango
  '' + genIniOrdered ([{
    name = "hydra";
    command = scripts.hydra-status;
    interval = 30;
  }] ++ optional (config.secrets ? gmail) {
    name = "email";
    command = scripts.email;
  } ++ [
    {
      name = "weather";
      command = scripts.weather;
      interval = 600;
    }
    {
      name = "calendar";
      command = scripts.calendar;
    }
    {
      command = scripts.music;
      interval = 1;
    }
    {
      command = scripts.sound;
      interval = 1;
    }
  ] ++ optionals config.deviceSpecific.isLaptop [
    {
      name = "battery";
      command = scripts.battery;
    }
    {
      name = "brightness";
      command = scripts.brightness;
      interval = 1;
    }
  ] ++ [
    {
      name = "connections";
      command = scripts.connections;
    }
    {
      name = "network";
      command = scripts.network;
    }
    {
      name = "network";
      command = ''
        top -b -n1 -p 1 | fgrep "Cpu(s)" | tail -1 | awk -F'id,' -v prefix="$prefix" '{ split($1, vs, ","); v=vs[length(vs)]; sub("%", "", v); printf "%s%.1f%%\n", prefix, 100 - v }' '';
      interval = 3;
    }
    {
      name = "freq";
      command = ''
        echo $(${pkgs.bc}/bin/bc -l <<< "scale=2; `cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_cur_freq|sort|tail -1`/1000000") GHz'';
    }
    {
      name = "frequency";
      command = scripts.temperature;
    }
    {
      name = "free";
      command = scripts.free;
    }
    {
      name = "df";
      command = ''
        echo '<span font="Material Icons 11"></span>' `df / | tail -1 | grep -o '..%'`'';
    }
    {
      name = "calendar";
      command =
        "${pkgs.coreutils}/bin/date +'<span font=\"Material Icons 11\"></span> %a %y-%m-%d'";
      interval = 600;
    }
    {
      name = "time";
      command =
        "${pkgs.coreutils}/bin/date +'<span font=\"Material Icons 11\"></span> %T'";
      interval = 1;
    }
  ]);
}
