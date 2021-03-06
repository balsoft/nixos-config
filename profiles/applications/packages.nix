{ pkgs, config, lib, inputs, ... }: {
  home-manager.users.balsoft.home.packages = with pkgs;
    [
      # Internet
      wget
      curl
      unrar
      neochat
    ] ++ lib.optionals config.deviceSpecific.goodMachine [
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
      pavucontrol
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
      xdg_utils
      lambda-launcher
      nix-patch
      gopass
      papirus-icon-theme
      shellcheck
      proselint
    ];
}
