{ pkgs, config, lib, inputs, ... }: {
  home-manager.users.balsoft.home.packages = with pkgs;
    [
      # Internet
      wget
      curl

      eza
      jq
    ] ++ lib.optionals config.deviceSpecific.goodMachine [
      # steamcmd
      # steam
      haskellPackages.hoogle
      nixfmt
      # nil
      nixpkgs-fmt
      stdman
      libqalculate
      # Messaging
      libnotify
      # Audio/Video
      mpv
      vlc
      pavucontrol
      # Tools
      zip
      plasma-systemmonitor
      wl-clipboard
      grim
      slurp
      abiword
      gnumeric
      gcalcli
      xdg-utils
      lambda-launcher
      nix-patch
      gopass
      # papirus-icon-theme
      breeze-icons
      shellcheck
      proselint
      ripgrep
      bat
      jless

      pandoc
      sioyek
    ];
}
