{ pkgs, config, ... }: {
  home-manager.users.balsoft = {
    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      pinentryFlavor = "gtk2";
    };
    programs.gpg = {
      enable = true;
      homedir = "${config.home-manager.users.balsoft.xdg.dataHome}/gnupg";
    };
  };
}
