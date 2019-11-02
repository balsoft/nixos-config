{ config, pkgs, lib, ... }: {
  home-manager.users.balsoft = {
    xsession.windowManager.i3.config.startup = [{
      command = "${pkgs.kanshi}/bin/kanshi";
      notification = false;
      always = true;
    }];
    xdg.configFile."kanshi/config".text = ''
      {
        output eDP-1 position 0,500 scale 1.2
        output HDMI-A-2 serial G6LMRS048669 position 1600,0 scale 1.0
      }
    '';
  };
}
