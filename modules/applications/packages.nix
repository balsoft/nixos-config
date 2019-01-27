{pkgs, config, lib, ...}:
{
  
  programs.wireshark.enable = true;
  
  programs.adb.enable = true;
  
  environment.systemPackages = (with pkgs.kdeApplications; [
    ark dolphin dolphin-plugins dragon eventviews ffmpegthumbs
    filelight gwenview kcachegrind kcalc kcolorchooser kdenlive
    kleopatra kolourpaint kompare krdc krfb kruler ktnef kwalletmanager
    marble okteta okular print-manager kio-extras
  ])
  ++
  (builtins.filter pkgs.stdenv.lib.isDerivation (builtins.attrValues (pkgs.plasma5)))
  ++ (with pkgs; [
    kded
    kdeFrameworks.kio
    plasma-integration
    kinit
    plasma5.xdg-desktop-portal-kde
    termNote
    stdman
    stdmanpages
  ]);

  
  home-manager.users.balsoft.home.packages = 
  (with pkgs; [
    # Internet
    wget
    curl
  ] ++ lib.optionals config.deviceSpecific.goodMachine [
    geany
    kdevelop
    kate
    texlive.combined.scheme-full
    steam
    krita
    kdenlive
    frei0r
    ffmpeg-full
    ghc
    chromium
    arduino
    cmake
    gnumake
    gcc
    gdb
    kile
  ] ++ [
    firefox
    # Messaging
    tdesktop
    telepathy_haze
    telepathy_idle
    libnotify
    # Audio/Video
    vlc
    google-play-music-desktop-player
    lxqt.pavucontrol-qt
    # Tools
    zip
    unrar
    wine
    kolourpaint
    ktorrent
    wireshark
    #wpsoffice
    micro
    python3
    qalculate-gtk
    breeze-qt5
    gnome3.adwaita-icon-theme
    papirus-icon-theme
    breeze-icons
    ksysguard
    scrot
    xclip
    abiword
    gnumeric
    gcalcli
    google-drive-ocamlfuse
    kdeconnect
    trojita
    nix-zsh-completions
    material-icons
    papirus-icon-theme
    kde-cli-tools
    vk
  ]) ;


}
