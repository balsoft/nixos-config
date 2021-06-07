{ pkgs, config, ... }: {
  services.pcscd.enable = true;
  services.udev.packages = [ pkgs.yubikey-personalization ];

  home-manager.users.balsoft = {
    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      pinentryFlavor = "gtk2";
    };

    programs.gpg = {
      enable = true;
      homedir = "${config.home-manager.users.balsoft.xdg.dataHome}/gnupg";
      scdaemonSettings = {
        disable-ccid = true;
        reader-port = "Yubico Yubi";
      };
    };
  };
}
