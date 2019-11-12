{ pkgs, config, lib, ... }:
with import ../../../support.nix { inherit pkgs config lib; };
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
  '' + genIni {
    A_hydra = {
      command = scripts.hydra-status;
      interval = 30;
    };
    a_email = if config.secrets ? gmail then {
      command = scripts.email;
    } else
      { };
    b_weather = {
      command = scripts.weather;
      interval = 600;
    };
    c_calendar = { command = scripts.calendar; };
    d_sound = {
      command = scripts.sound;
      interval = 1;
    };
    e_music = {
      command = scripts.music;
      interval = 1;
    };
    f_battery = pkgs.stdenv.lib.optionalAttrs config.deviceSpecific.isLaptop {
      command = scripts.battery;
    };
    g_brightness =
      pkgs.stdenv.lib.optionalAttrs config.deviceSpecific.isLaptop {
        command = scripts.brightness;
        interval = 1;
      };
    h_wireless = lib.optionalAttrs (config.deviceSpecific.isLaptop) {
      command = scripts.wireless;
    };
    i_network = { command = scripts.network; };
    j_cpuload = {
      command = ''
        top -b -n1 -p 1 | fgrep "Cpu(s)" | tail -1 | awk -F'id,' -v prefix="$prefix" '{ split($1, vs, ","); v=vs[length(vs)]; sub("%", "", v); printf "%s%.1f%%\n", prefix, 100 - v }' '';
      interval = 3;
    };
    k_cpufreq = {
      command = ''
        echo $(${pkgs.bc}/bin/bc -l <<< "scale=2; `cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_cur_freq|sort|tail -1`/1000000") GHz'';
    };
    l_temperature = { command = scripts.temperature; };
    m_free = { command = scripts.free; };
    n_df = {
      command = ''
        echo '<span font="Material Icons 11"></span>' `df / | tail -1 | grep -o '..%'`'';
    };
    o_date = {
      command =
        "${pkgs.coreutils}/bin/date +'<span font=\"Material Icons 11\"></span> %a %y-%m-%d'";
      interval = 10;
    };
    p_time = {
      command =
        "${pkgs.coreutils}/bin/date +'<span font=\"Material Icons 11\"></span> %T'";
      interval = 1;
    };
  };
}
