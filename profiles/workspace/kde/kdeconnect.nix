{ lib, ... }: {
  home-manager.users.balsoft = {
    services.kdeconnect.enable = true;
    systemd.user.services.kdeconnect = {
      Service.Environment = lib.mkForce [
        "PATH=/etc/profiles/per-user/balsoft/bin"
      ];
    };
  };
  persist.state.directories = [ "/home/balsoft/.config/kdeconnect" ];
}
