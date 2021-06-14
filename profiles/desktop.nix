{ inputs, ... }: {
  imports = with inputs.self.nixosModules; [
    ./base.nix

    applications-setup
    ezwg
    hardware
    themes
    virtualisation

    alacritty
    emacs
    firefox
    geary
    github
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
    simple-osd-daemons
    sway
    xresources
  ];
}
