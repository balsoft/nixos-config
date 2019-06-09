{ pkgs, config, lib, ... }: {
  home-manager.users.balsoft = {
    xdg.configFile."rclone/rclone.conf.home".text = config.secrets.rclone;
    home.activation."rclone" = {
      after = [ "linkGeneration" ];
      before = [ ];
      data = ''
        cp ./.config/rclone/rclone.conf.home ./.config/rclone/rclone.conf
        chmod 700 ./.config/rclone/rclone.conf
        mkdir -p cloud; cd cloud
        mkdir -p 'Google Drive' 'Yandex Disk' 'Dropbox'
      '';
    };
    xsession.windowManager.i3.config.startup = [
      {
        command =
        "${pkgs.rclone}/bin/rclone mount google:/ '/home/balsoft/cloud/Google Drive' --daemon";
      }
      {
        command =
        "${pkgs.rclone}/bin/rclone mount Yandex:/ '/home/balsoft/cloud/Yandex Disk' --daemon";
      }
      {
        command =
        "${pkgs.rclone}/bin/rclone mount Dropbox:/ '/home/balsoft/cloud/Dropbox' --daemon";
      }
    ];
  };
}
