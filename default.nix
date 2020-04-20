# This is balsoft's configuration file.
#
# https://github.com/balsoft/nixos-config
#
# This is main nixos configuration
# To use this configuration:
#   1. Add your own secret.nix and hardware-configuration/`hostname`.nix to this folder
#   2. Set the hostname to the desired one
#   3. ./install or ./bootstrap
#   4. Log in to application and services where neccesary

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
