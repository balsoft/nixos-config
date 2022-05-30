{ inputs, ... }: {
  imports = with inputs.self.nixosModules; with inputs.self.nixosProfiles; [
    ./base.nix

    # MODULES
    themes
    ezwg


    # PROFILES
    applications-setup
    bluetooth
    power
    opengl
    hardware
    sound
    virtualisation

    alacritty
    # cantata
    # emacs
    vscodium
    firefox
    geary
    github
    gwenview
    himalaya
    nheko
    packages

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
  ];
}
