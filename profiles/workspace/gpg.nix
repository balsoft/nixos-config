{ pkgs, lib, config, ... }: {
  services.pcscd.enable = true;
  services.udev.packages = [ pkgs.yubikey-personalization ];

  persist.derivative.directories = [ "/home/balsoft/.local/share/gnupg" ];

  home-manager.users.balsoft = {
    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      pinentryPackage = pkgs.pinentry-qt;
    };

    systemd.user.services.gpg-agent = {
      Service = {
        Environment = lib.mkForce [
          "GPG_TTY=/dev/tty1"
          "DISPLAY=:0"
          "GNUPGHOME=${config.home-manager.users.balsoft.xdg.dataHome}/gnupg"
        ];
      };
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
