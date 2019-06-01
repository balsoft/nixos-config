{ pkgs, lib, config, ... }: {
  home-manager.users.balsoft = {
    programs.autorandr.hooks.preswitch.compton = "pkill compton";
    programs.autorandr.hooks.postswitch.compton =
    "allow_rgb10_configs=false ${pkgs.compton}/bin/compton";
    services.compton = {
      enable = true;
      backend = "xr-glx-hybrid";
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
