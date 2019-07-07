#!/usr/bin/env nix-shell
#!nix-shell -p git -i bash

IN_NIX_SHELL=

imports=$(pwd)

for author in `ls $imports/github`
do
    for repo in `ls "$imports/github/$author"`
    do
        cd "$imports/github/$author"
        git init "./$repo"
        git submodule add --force "https://github.com/$author/$repo" "./$repo"
        git submodule init
        cd "$repo"
        git remote add origin "https://github.com/$author/$repo"
        git fetch
        git checkout master
        git pull
    done
done

rm $imports/nixpkgs
ln -s $imports/github/nixos/nixpkgs-channels $imports/nixpkgs

cd $imports/nixpkgs
git checkout nixos-19.03
git pull

cd $imports/..

git add .
git commit -m "Update versions of imports"

