{ pkgs, lib, config, ... }: {
  home-manager.users.balsoft = {
    programs.autorandr.hooks.preswitch.compton = "pkill compton";
    programs.autorandr.hooks.postswitch.compton = "allow_rgb10_configs=false ${
      pkgs.compton
    }/bin/compton -c -C --backend=glx --vsync=opengl --shadow-exclude='!(I3_FLOATING_WINDOW@:c = 1)'";
  };
}
