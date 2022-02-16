{ config, pkgs, ... }: {
  environment.systemPackages = [ pkgs.clipman ];
  home-manager.users.balsoft = {
    wayland.windowManager.sway.config.startup =
      [{ command = "${pkgs.wl-clipboard}/bin/wl-paste -t text --watch ${pkgs.clipman}/bin/clipman store"; }];
  };
}
