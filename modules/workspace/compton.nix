{ pkgs, lib, config, ... }: {
  home-manager.users.balsoft = {
    programs.autorandr.hooks.preswitch.compton = "pkill compton";
    programs.autorandr.hooks.postswitch.compton = "allow_rgb10_configs=false ${
      pkgs.compton
    }/bin/compton --backend xr-glx-hybrid -i 0 --vsync opengl-swc -cC --shadow-exclude '!focused' &";
  };
}
