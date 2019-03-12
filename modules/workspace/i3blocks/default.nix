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
    e_battery =
    {
      command = scripts.battery;
    };
    f_wireless = 
    {
      command = scripts.wireless;
    };
    g_cpuload =
    {
      command = ''top -b -n2 -p 1 | fgrep "Cpu(s)" | tail -1 | awk -F'id,' -v prefix="$prefix" '{ split($1, vs, ","); v=vs[length(vs)]; sub("%", "", v); printf "%s%.1f%%\n", prefix, 100 - v }' '';
    };
    h_cpufreq =
    {
      command = ''echo $(${pkgs.bc}/bin/bc -l <<< "scale=2; `cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_cur_freq|sort|head -1`/1000000") GHz'';
    };
    i_temperature =
    {
      command = scripts.temperature;
    };
    j_free = 
    {
      command = scripts.free;
    };
    k_date =
    {
      command = "${pkgs.coreutils}/bin/date +'<span font=\"Material Icons 11\"></span> %a %y-%m-%d'";
      interval = 10;
    };
    l_time =
    {
      command = "${pkgs.coreutils}/bin/date +'<span font=\"Material Icons 11\"></span> %T'";
      interval = 1;
    };
  };
}
