{ config, pkgs, lib, ... }: {
  home-manager.users.balsoft = {
    xsession.windowManager.i3.config.startup = [{
      command = "${pkgs.kanshi}/bin/kanshi";
      notification = false;
      always = true;
    }];
    xdg.configFile."kanshi/config".text = ''
      {
        output "Samsung Electric Company C27JG5x HTOM901267" position 0,1025
        output "Dell Inc. DELL E176FP MC0435BH2ATP" position 1000,0
      }
    '';
  };
}
