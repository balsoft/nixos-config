{pkgs, config, lib, ...}:
{
  home-manager.users.balsoft = {
    xdg.configFile."rclone/rclone.conf.home".text = config.secrets.rclone;
    home.activation."rclone" = {
      after = ["linkGeneration"];
      before = [];
      data = "cp ./.config/rclone/rclone.conf.home ./.config/rclone/rclone.conf; chmod 700 ./.config/rclone/rclone.conf";
    };
  };
}
