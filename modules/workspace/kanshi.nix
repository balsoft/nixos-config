{ config, pkgs, lib, ... }:
{
  home-manager.users.balsoft = 
  {
    # xsession.windowManager.i3.config.startup = 
    # [ {command = "${pkgs.kanshi}/bin/kanshi"; notification = false; always = true;} ];                           
    xdg.configFile."kanshi/config".text = 
    ''
    {
      output eDP-1 resolution 1920x1080 position 0,853 scale 1.5
      output HDMI-A-2 resolution 1920x1080 position 1280,853 scale 1.0
      output HDMI-A-1 vendor DEL serial MC0435BH2ATP resolution 1280x1024 position 0,0 scale 1.2
    }
    {
      output eDP-1 resolution 1920x1080 position 0,0 scale 1.0
      output *
    }
    {
      output eDP-1 resolution 1920x1080 position 0,0 scale 1.0
    }
    '';
  };
}
