{ pkgs, config, lib, ... }:
with import ../../../support.nix {inherit pkgs config lib;};
let scripts = import ./scripts pkgs config; in
{
  home-manager.users.balsoft.xdg.configFile."i3blocks/config".text = 
  ''
  interval=60
  markup=pango
  ''
  +
  genIni
  {
    a_email = if ! isNull config.secrets.gmail or null then
    {
      command = scripts.email;
    } else {};
    b_weather = 
    {
      command = "curl -Ss 'https://wttr.in?0&T&Q' | cut -c 16- | head -2 | xargs echo";
      interval = 3600;
    };
    c_calendar =
    {
      command = scripts.calendar;
    };
    d_sound = 
    {
      command = scripts.sound;
      interval = 1;
    };
    e_music = 
    {
      command = scripts.music;
      interval = 1;
    };
    f_battery =
    pkgs.stdenv.lib.optionalAttrs config.deviceSpecific.isLaptop 
    {
      command = scripts.battery;
    };
    g_brightness =
    pkgs.stdenv.lib.optionalAttrs config.deviceSpecific.isLaptop 
    {
      command = scripts.brightness;
      interval = 1;
    };
    h_wireless = 
    {
      command = scripts.wireless;
    };
    i_cpuload =
    {
      command = ''top -b -n1 -p 1 | fgrep "Cpu(s)" | tail -1 | awk -F'id,' -v prefix="$prefix" '{ split($1, vs, ","); v=vs[length(vs)]; sub("%", "", v); printf "%s%.1f%%\n", prefix, 100 - v }' '';
      interval = 3;
    };
    j_cpufreq =
    pkgs.stdenv.lib.optionalAttrs config.deviceSpecific.isLaptop 
    {
      command = ''echo $(${pkgs.bc}/bin/bc -l <<< "scale=2; `cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_cur_freq|sort|head -1`/1000000") GHz'';
    };
    k_temperature =
    pkgs.stdenv.lib.optionalAttrs config.deviceSpecific.isLaptop 
    {
      command = scripts.temperature;
    };
    l_free = 
    {
      command = scripts.free;
    };
    m_date =
    {
      command = "${pkgs.coreutils}/bin/date +'<span font=\"Material Icons 11\"></span> %a %y-%m-%d'";
      interval = 10;
    };
    n_time =
    {
      command = "${pkgs.coreutils}/bin/date +'<span font=\"Material Icons 11\"></span> %T'";
      interval = 1;
    };
  };
}
