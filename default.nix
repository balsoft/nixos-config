# This is balsoft's configuration file.
#
# https://github.com/balsoft/nixos-config
#
# This is main nixos configuration
# To use this configuration:
#   1. Add your own secret.nix and hardware-configuration/`hostname`.nix to this folder
#   2. Set the hostname to the desired one
#   3. `sudo nixos-rebuild switch --flake .`
#   4. Log in to application and services where neccesary

{ config, pkgs, lib, inputs, name, ... }:
rec {
  imports = [
    (./hardware-configuration + "/${name}.nix")
    inputs.home-manager.nixosModules.home-manager
    (import ./modules device)
  ];

  device = name;

  system.stateVersion = "18.03";
}
