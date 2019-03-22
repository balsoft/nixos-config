#!/usr/bin/env nix-shell
#!nix-shell -p p7zip git -i bash

if [[ -z ~/.local/share/password ]]
then
    echo -n "Password [echoed]: "
    read > ~/.local/share/password
fi

7z e secret.nix.zip -y -p`cat ~/.local/share/password`

git submodule update --init --recursive

export NIX_PATH=nixpkgs=./imports/nixpkgs:nixos-config=./ 

nix build -f ./imports/nixpkgs/nixos system &&
{
    git tag "Builds at `date +D%FT%r`"
    sudo nixos-rebuild switch
}

