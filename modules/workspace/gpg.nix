{
  home-manager.users.balsoft = {
    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      pinentryFlavor = "gtk2";
    };
    programs.gpg.enable = true;
  };
}
