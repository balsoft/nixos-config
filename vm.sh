#!/usr/bin/env bash
[[ -z ./secret.nix ]] && echo null > secret.nix
nixos-rebuild build-vm -I nixpkgs=./imports/nixpkgs -I nixos-config=./vm.nix
./result/bin/run-NixOS-VM-vm -m size=4G # Sorry, my apps are way too large

