{ inputs, ... }: {
  imports = with inputs.self.nixosProfiles; [
    ./base.nix

    # PROFILES
    applications-setup
    bluetooth
    power
    opengl
    hardware
    sound
    virtualisation

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
    openvpn
    simple-osd-daemons
    sway
    yubikey-touch-detector
  ];
}
