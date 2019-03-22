#!/usr/bin/env nix-shell
#!nix-shell -p p7zip git -i bash

if [[ ! -e ~/.local/share/password ]]
then
    echo -n "Password [echoed]: "
    read password
    echo $password > ~/.local/share/password
fi

7z e secret.nix.zip -y -p`cat ~/.local/share/password`

git submodule update --init --recursive

export NIX_PATH=nixpkgs=./imports/nixpkgs:nixos-config=/etc/nixos/configuration.nix 

nix build -f ./imports/nixpkgs/nixos system &&
{
    git tag "Build`date +%F`"
    sudo nixos-rebuild switch
}

