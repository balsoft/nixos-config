{ inputs, ... }: {
  imports = with inputs.self.nixosProfiles; [
    ./base.nix

    boot

    # PROFILES
    applications-setup
    bluetooth
    power
    opengl
    hardware
    sound
    virtualisation

    yubikey
    vlock

    alacritty
    aerc
    # cantata
    # emacs
    # vscodium
    helix
    firefox
    # geary
    github
    gwenview
    himalaya
    nheko
    packages
    okular

    pass-secret-service
    copyq
    cursor
    direnv
    fonts
    gnome3
    gtk
    i3blocks
    qt
    slack
    kdeconnect
    light
    mako
    # mopidy
    simple-osd-daemons
    sway
    yubikey-touch-detector
    btop
  ];
}
