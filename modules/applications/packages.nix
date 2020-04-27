{ pkgs, config, lib, ... }: {
  programs.adb.enable = true;

  environment.systemPackages = with pkgs;
    lib.optionals config.deviceSpecific.goodMachine [
      kded
      ntfs3g
    ];

  home-manager.users.balsoft.home.packages = with pkgs;
    [
      # Internet
      wget
      curl
      unrar
    ] ++ lib.optionals config.deviceSpecific.goodMachine ([
      steamcmd
      steam
      kdenlive
      frei0r
      ffmpeg-full
      ghc
      haskellPackages.hoogle
      clang
      clang-tools
      lldb
      appimage-run
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
      wireshark
      micro
      python3
      qalculate-gtk
      breeze-qt5
      gnome3.adwaita-icon-theme
      papirus-icon-theme
      breeze-icons
      ksysguard
      wl-clipboard
      grim
      slurp
      abiword
      gnumeric
      gcalcli
      google-drive-ocamlfuse
      kdeconnect
      trojita
      nix-zsh-completions
      papirus-icon-theme
      breeze-icons
      kde-cli-tools
      xdg_utils
      nheko
      gitAndTools.hub
      git-crypt
      nix-patch
      waypipe
      cachix
    ] ++ (with pkgs.kdeApplications; [
      ark
      dolphin
      dolphin-plugins
      eventviews
      ffmpegthumbs
      filelight
      gwenview
      kcachegrind
      kcolorchooser
      kdenlive
      kolourpaint
      kompare
      krdc
      krfb
      ktnef
      marble
      okteta
      okular
      print-manager
      ktorrent
    ]));
}
