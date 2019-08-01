#!/usr/bin/env nix-shell
#!nix-shell -p git -i bash

RELEASE=19.03

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
ln -s $imports/github/nixos/nixpkgs $imports/nixpkgs

cd $imports/nixpkgs
git checkout release-$RELEASE
git pull

cd $imports

curl -L https://nixos.org/channels/nixos-$RELEASE/nixexprs.tar.xz | tar -OJx --wildcards "*/programs.sqlite" > programs.sqlite

git add .
git commit -m "Update versions of imports"

