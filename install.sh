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

nixos-rebuild build &&
    {
        git add .
        git commit -m "Automatic commit. This builds at `date`"
        git tag latestBuild --force
        sudo nixos-rebuild switch
    }

