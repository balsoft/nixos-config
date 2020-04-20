# This is balsoft's configuration file.
#
# https://github.com/balsoft/nixos-config
#
# This is main nixos configuration
# To use this configuration:
#   1. Add your own secret.nix to this folder
#   2. ./install or ./bootstrap
#   3. Log in to application and services where neccesary

{ config, pkgs, lib, ... }:
let sources = import ./nix/sources.nix;
in rec {
  imports = [
    "${./hardware-configuration}/${device}.nix"
    "${sources.home-manager}/nixos"
    (import ./modules device)
  ];

  device = builtins.replaceStrings ["\n"] [""] (builtins.readFile /etc/hostname);

  system.stateVersion = "18.03";
}
