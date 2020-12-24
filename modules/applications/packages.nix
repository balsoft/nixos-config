{ pkgs, config, lib, inputs, ... }: {
  home-manager.users.balsoft.home.packages = with pkgs;
    [
      # Internet
      wget
      curl
      unrar
    ] ++ lib.optionals config.deviceSpecific.goodMachine ([
      steamcmd
      steam
      haskellPackages.hoogle
      nixfmt
      niv
      stdman
      libqalculate
      # Messaging
      libnotify
      # Audio/Video
      vlc
      cantata
      lxqt.pavucontrol-qt
      # Tools
      zip
      unrar
      ksysguard
      wl-clipboard
      grim
      slurp
      abiword
      gnumeric
      gcalcli
      breeze-icons
      kde-cli-tools
      xdg_utils
      git-crypt
      inputs.yt-utilities.defaultPackage.x86_64-linux
      lambda-launcher
      nix-patch
      pass-wayland
      papirus-icon-theme
      gnome3.simple-scan
    ]);
}
