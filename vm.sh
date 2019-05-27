#!/usr/bin/env bash
[[ -z ./secret.nix ]] && echo null > secret.nix
NIX_PATH=nixpkgs=./imports/nixpkgs:nixos-config=./vm.nix nix build -f ./imports/nixpkgs/nixos vm
./result/bin/run-NixOS-VM-vm -m size=4G # Sorry, my apps are way too large

