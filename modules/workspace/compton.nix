{ pkgs, lib, config, ... }: {
  home-manager.users.balsoft = {
    programs.autorandr.hooks.preswitch.compton = "systemctl --user stop compton";
    programs.autorandr.hooks.postswitch.compton = "systemctl --user start compton";
    services.compton = {
      enable = true;
      backend = "glx";
      blur = true;
      inactiveOpacity = toString 0.9;
      menuOpacity = toString 0.85;
      noDNDShadow = false;
      shadow = true;
      shadowExclude = [ "!(I3_FLOATING_WINDOW@:c = 1)" ];
      vSync = "opengl-swc";
    };
  };
}
