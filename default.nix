# This is balsoft's configuration file.
#
# https://github.com/balsoft/nixos-config
#
# This is main nixos configuration
# To use this configuration:
#   1. Add your own secret.nix to this folder
#   2. Replace /etc/nixos/configuration.nix with the following:
#      import /path/to/this/nixos-config "Vendor-Type"
#   3. Log in to application and services where neccesary

device: # This is the device we're on now
{ config, pkgs, lib, ... }:
let sources = import ./nix/sources.nix;
in {
  imports = [
    /etc/nixos/hardware-configuration.nix
    "${sources.home-manager}/nixos"
    "${sources.simple-nixos-mailserver}"
    ./modules
  ];

  inherit device;

  system.stateVersion = "18.03";
}
