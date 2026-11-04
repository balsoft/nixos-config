{ pkgs, lib, config, ... }: {
  services.pcscd.enable = true;
  services.udev.packages = [ pkgs.yubikey-personalization ];

  persist.derivative.homeDirectories = [ ".local/share/gnupg" ];

  home-manager.users.balsoft = {
    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      extraConfig = ''
        pinentry-program ${pkgs.writeShellScript "pinentry" ''
          if [[ $TTY =~ '/dev/tty.*' ]]; then
            ${pkgs.pinentry-curses}/bin/pinentry-curses
          else
            ${pkgs.pinentry-qt}/bin/pinentry-qt
          fi
        ''}
      '';
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
