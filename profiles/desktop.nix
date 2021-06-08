{ inputs, ... }: {
  imports = with inputs.self.nixosModules; [
    ./base.nix

    applications
    ezwg
    hardware
    power
    themes
    virtualisation

    alacritty
    emacs
    firefox
    geary
    himalaya
    nheko
    packages
    yt-utilities

    cursor
    fonts
    gnome3
    gtk
    i3blocks
    kde
    light
    mako
    simple-osd-daemons
    sway
    xresources
  ];
}
