device:
{ pkgs, lib, ... }: {
  imports = [
    ./applications/packages.nix
    ./applications/emacs
    ./applications/alacritty.nix
    ./applications/geary.nix
    ./applications/weechat.nix
    ./applications/okular.nix
    ./applications/yt-utilities.nix
    ./applications/firefox.nix
    ./applications/spectral.nix
    ./workspace/sway
    ./workspace/i3blocks
    ./workspace/zsh.nix
    ./workspace/gtk.nix
    ./workspace/gnome3
    ./workspace/misc.nix
    ./workspace/kde
    ./workspace/ssh.nix
    ./workspace/locale.nix
    ./workspace/fonts.nix
    ./workspace/light.nix
    ./workspace/mako.nix
    ./workspace/mopidy.nix
    ./workspace/xresources.nix
    ./themes.nix
    ./applications.nix
    ./secrets.nix
    ./secrets-envsubst.nix
    ./devices.nix
    ./packages.nix
    ./users.nix
    ./hardware.nix
    ./services.nix
    ./power.nix
    ./network.nix
    ./simple-osd-daemons.nix
  ] ++ lib.optionals (device == "AMD-Workstation") [
    ./nextcloud.nix
    ./mailserver.nix
    ./matrix-synapse.nix
    # ./workspace/kanshi.nix
    ./nginx.nix
    ./gitea.nix
    ./minidlna.nix
  ];
}
