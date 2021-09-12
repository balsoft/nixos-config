{ inputs, ... }: {
  imports = with inputs.self.nixosModules; with inputs.self.nixosProfiles; [
    ./base.nix

    # MODULES
    themes
    ezwg


    # PROFILES
    applications-setup
    hardware
    virtualisation

    alacritty
    cantata
    emacs
    firefox
    geary
    github
    gwenview
    himalaya
    nheko
    packages
    yt-utilities

    cursor
    direnv
    fonts
    gnome3
    gtk
    i3blocks
    kde
    light
    mako
    mopidy
    simple-osd-daemons
    sway
  ];
}
