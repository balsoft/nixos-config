{pkgs, config, lib, ...}:
{
  programs.adb.enable = true;
  
  environment.systemPackages = (with pkgs.kdeApplications; [
    ark dolphin dolphin-plugins dragon eventviews ffmpegthumbs
    filelight gwenview kcachegrind kcalc kcolorchooser kdenlive
    kolourpaint kompare krdc krfb kruler ktnef
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
    qt5.qtsvg
  ]);

  
  home-manager.users.balsoft.home.packages = 
  (with pkgs; [
    # Internet
    wget
    curl
  ] ++ lib.optionals config.deviceSpecific.goodMachine [
    texlive.combined.scheme-full
    steam
    krita
    kdenlive
    frei0r
    ffmpeg-full
    ghc
    haskellPackages.hoogle
    haskellPackages.hindent
    arduino
    kile
    clang
    lldb
    #gcc
    #gdb
    appimage-run
  ] ++ [
    firefox
    # Messaging
    tdesktop
    telepathy_haze
    telepathy_idle
    libnotify
    # Audio/Video
    vlc
    cantata
    lxqt.pavucontrol-qt
    # Tools
    zip
    unrar
    wine
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
    breeze-icons
    kde-cli-tools
    vk-messenger
    xdg_utils
  ]) ;


}
