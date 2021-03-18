{ pkgs, ... }: {
  services.dbus.packages = [ pkgs.gcr ];
  home-manager.users.balsoft = {
    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      pinentryFlavor = "gnome3";
    };
    programs.gpg.enable = true;
  };
}
