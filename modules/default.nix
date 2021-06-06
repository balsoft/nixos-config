builtins.listToAttrs (builtins.map (path: {
  name = builtins.head (let
    b = builtins.baseNameOf path;
    m = builtins.match "(.*)\\.nix" b;
  in if isNull m then [ b ] else m);
  value = import path;
}) [
  ./applications.nix
  ./applications/alacritty.nix
  ./applications/emacs
  ./applications/firefox.nix
  ./applications/geary.nix
  ./applications/himalaya.nix
  ./applications/okular.nix
  ./applications/packages.nix
  ./applications/yt-utilities.nix
  ./boot.nix
  ./devices.nix
  ./ezwg.nix
  ./hardware.nix
  ./network.nix
  ./nix.nix
  ./overlay.nix
  ./power.nix
  ./secrets-envsubst.nix
  ./secrets.nix
  ./security.nix
  ./servers/gitea.nix
  ./servers/home-assistant.nix
  ./servers/jitsi.nix
  ./servers/mailserver.nix
  ./servers/mastodon.nix
  ./servers/matrix-synapse.nix
  ./servers/minidlna.nix
  ./servers/nextcloud.nix
  ./servers/nginx.nix
  ./servers/vsftpd.nix
  ./services.nix
  ./themes.nix
  ./virtualisation.nix
  ./workspace/cursor.nix
  ./workspace/fonts.nix
  ./workspace/git.nix
  ./workspace/gnome3
  ./workspace/gpg.nix
  ./workspace/gtk.nix
  ./workspace/i3blocks
  ./workspace/kde
  ./workspace/light.nix
  ./workspace/locale.nix
  ./workspace/mako.nix
  ./workspace/misc.nix
  ./workspace/simple-osd-daemons.nix
  ./workspace/ssh.nix
  ./workspace/sway
  ./workspace/xresources.nix
  ./workspace/zsh.nix
])
