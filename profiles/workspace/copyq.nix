{ config, pkgs, ... }: {
  environment.systemPackages = [ pkgs.copyq ];
  home-manager.users.balsoft = {
    wayland.windowManager.sway.config = {
      window.commands = [
        {
          criteria = { title = ".*CopyQ"; };
          command = "floating enable";
        }
        {
          criteria = { title = ".*CopyQ"; };
          command = "move position mouse";
        }
      ];
      startup = [{ command = "${pkgs.copyq}/bin/copyq"; }];
    };

  };
}
