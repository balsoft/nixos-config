{pkgs, lib, config, ...}:
{
  home-manager.users.balsoft =
  {
    programs.autorandr.hooks.postswitch.compton =  "pkill compton; allow_rgb10_configs=false ${pkgs.compton}/bin/compton --backend xr-glx-hybrid -i 0 --vsync opengl-swc -C --shadow-exclude '!focused' --shadow-exclude-reg 'x${builtins.elemAt (builtins.split "px" config.home-manager.users.balsoft.services.polybar.config."bar/top".height) 0}+0+0' &";
  };
}
