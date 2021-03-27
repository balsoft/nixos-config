{ inputs, ... }: {
  imports = with inputs.self.nixosModules; [
    ./base.nix

    applications
    ezwg
    hardware
    power
    services
    themes
    virtualisation

    alacritty
    emacs
    firefox
    geary
    packages
    weechat
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
