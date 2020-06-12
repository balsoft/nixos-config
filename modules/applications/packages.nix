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
      nheko
      git-crypt
      nix-patch
      inputs.yt-utilities.defaultPackage.x86_64-linux
      pass-wayland
    ] ++ (with pkgs.kdeApplications; [
      ark
      dolphin
      dolphin-plugins
      gwenview
      kcachegrind
      kcolorchooser
      kdenlive
      kolourpaint
      marble
      okular
      print-manager
    ]));
}
