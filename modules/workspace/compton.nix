{pkgs, lib, config, ...}:
{
  home-manager.users.balsoft =
  {
    programs.autorandr.hooks.predetect.compton = "pkill compton";
    programs.autorandr.hooks.postswitch.compton =  "allow_rgb10_configs=false ${pkgs.compton}/bin/compton --backend glx -i 0 --vsync opengl-swc -C --shadow-exclude '!focused' --shadow-exclude-reg 'x${builtins.elemAt (builtins.split "px" config.home-manager.users.balsoft.services.polybar.config."bar/top".height) 0}+0+0' &";
  };
}