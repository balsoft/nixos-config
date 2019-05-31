{ pkgs, lib, config, ... }: {
  home-manager.users.balsoft = {
    programs.autorandr.hooks.preswitch.compton = "pkill compton";
    programs.autorandr.hooks.postswitch.compton = "allow_rgb10_configs=false ${
      pkgs.compton
    }/bin/compton -c -C --vsync opengl-swc --shadow-exclude '!(I3_FLOATING_WINDOW@:c = 1)' --shadow-exclude-reg 1920x24+0+0";
  };
}
