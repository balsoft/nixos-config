#!/usr/bin/env nix-shell
#!nix-shell -p p7zip git -i bash

firsttime=false

if [[ $1 == "--first-time" ]]
then
    firsttime=true
    shift
    hostname=$1
    shift
fi

unset IN_NIX_SHELL

if [[ ! -e ~/.local/share/password ]]
then
    echo -n "Password [echoed]: "
    read password
    echo $password > ~/.local/share/password
fi

7z e secret.nix.zip -y -p`cat ~/.local/share/password`

git submodule update --init --recursive

export NIX_PATH=nixpkgs=./imports/nixpkgs:nixos-config=/etc/nixos/configuration.nix

if $firsttime
then
    umount /home
    nixos-generate-config --root /mnt
    echo "import /home/balsoft/projects/nixos-config \"$hostname\"" > /mnt/etc/nixos/configuration.nix
    mount --rbind /mnt/home /home/
    cp /mnt/etc/nixos/* /etc/nixos
    nix build -f ./imports/nixpkgs/nixos system $@ &&
    nixos-install --system ./result
else
    nix build -f ./imports/nixpkgs/nixos system $@ &&
        {
            git add .
            d=$(date +%s)
            git commit -m "Automatic commit. This builds at $d"
            git tag latestBuild --force
            dir=$(pwd)
            export SHELL=/bin/sh 
            pkexec nix-env --profile /nix/var/nix/profiles/system --set $(readlink $dir/result)
            pkexec $dir/result/bin/switch-to-configuration switch
        }
fi
