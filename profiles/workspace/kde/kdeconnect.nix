{
  home-manager.users.balsoft = {
    services.kdeconnect.enable = true;
  };
  persist.state.directories = [ "/home/balsoft/.config/kdeconnect" ];
}
