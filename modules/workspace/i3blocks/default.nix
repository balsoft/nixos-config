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
    a_weather = 
    {
      command = "curl -Ss 'https://wttr.in?0&T&Q' | cut -c 16- | head -2 | xargs echo";
      interval = 3600;
    };
    b_battery =
    {
      command = scripts.battery;
    };
    c_datetime =
    {
      command = "${pkgs.coreutils}/bin/date +'%y-%m-%d %a %T'";
      interval = 1;
    };
  };
}
