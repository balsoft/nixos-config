{ config, pkgs, lib, ... }: {
  home-manager.users.balsoft = {
    xsession.windowManager.i3.config.startup = [{
      command = "${pkgs.kanshi}/bin/kanshi";
      notification = false;
      always = true;
    }];
    xdg.configFile."kanshi/config".text = ''
      {
        output "Chimei Innolux Corporation 0x1361 0x00000000" mode 1920x1080 position 0,500 scale 1.2
        output "Unknown VZ249 G6LMRS048669" mode 1920x1080 position 1600,0 scale 1.0
      }
    '';
  };
}
