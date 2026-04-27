{ pkgs, config, lib, inputs, ... }: {
  home-manager.users.balsoft.home.packages = with pkgs;
    [
      # Internet
      wget
      curl

      eza
      jq

      file

      cliphist
    ] ++ lib.optionals config.deviceSpecific.goodMachine [
      # steamcmd
      # steam
      # haskellPackages.hoogle
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
      # Tools
      p7zip
      wl-clipboard
      grim
      slurp
      xdg-utils
      lambda-launcher
      gopass
      # papirus-icon-theme
      kdePackages.breeze-icons
      ripgrep
      bat
      jless

      pandoc

      nil
    ];
}
