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
    pkgs.stdenv.lib.optionalAttrs config.deviceSpecific.isLaptop 
    {
      command = scripts.battery;
    };
    f_brightness =
    pkgs.stdenv.lib.optionalAttrs config.deviceSpecific.isLaptop 
    {
      command = scripts.brightness;
      interval = 1;
    };
    g_wireless = 
    {
      command = scripts.wireless;
    };
    h_cpuload =
    {
      command = ''top -b -n2 -p 1 | fgrep "Cpu(s)" | tail -1 | awk -F'id,' -v prefix="$prefix" '{ split($1, vs, ","); v=vs[length(vs)]; sub("%", "", v); printf "%s%.1f%%\n", prefix, 100 - v }' '';
    };
    i_cpufreq =
    pkgs.stdenv.lib.optionalAttrs config.deviceSpecific.isLaptop 
    {
      command = ''echo $(${pkgs.bc}/bin/bc -l <<< "scale=2; `cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_cur_freq|sort|head -1`/1000000") GHz'';
    };
    j_temperature =
    pkgs.stdenv.lib.optionalAttrs config.deviceSpecific.isLaptop 
    {
      command = scripts.temperature;
    };
    k_free = 
    {
      command = scripts.free;
    };
    l_date =
    {
      command = "${pkgs.coreutils}/bin/date +'<span font=\"Material Icons 11\"></span> %a %y-%m-%d'";
      interval = 10;
    };
    m_time =
    {
      command = "${pkgs.coreutils}/bin/date +'<span font=\"Material Icons 11\"></span> %T'";
      interval = 1;
    };
  };
}
