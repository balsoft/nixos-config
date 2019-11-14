{ config, pkgs, lib, ... }: {
  home-manager.users.balsoft = {
    xsession.windowManager.i3.config.startup = [{
      command = "${pkgs.kanshi}/bin/kanshi";
      notification = false;
      always = true;
    }];
    xdg.configFile."kanshi/config".text = ''
      {
        output "Chimei Innolux Corporation 0x1361 0x00000000" position 0,500 scale 1.5
        output "Unknown VZ249 G6LMRS048669" position 1280,0
      }
      {
        output "Unknown VZ249 G6LMRS048669" position 0,1024
        output "Dell Inc. DELL E176FP MC0435BH2ATP" position 640,0
      }
    '';
  };
}
