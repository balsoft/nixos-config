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
        git fetch
        git pull
    done
done

rm $imports/nixpkgs

rm $imports/nixpkgs -rf
rm $imports/nixpkgs-unstable -rf

cd $imports/github/nixos/nixpkgs-channels

git worktree add --track -b nixos-$RELEASE ../../../nixpkgs origin/nixos-$RELEASE
git worktree add  ../../../nixpkgs-unstable origin/nixos-unstable

cd $imports

curl -L https://nixos.org/channels/nixos-$RELEASE/nixexprs.tar.xz | tar -OJx --wildcards "*/programs.sqlite" > programs.sqlite

git add .
git commit -m "Update versions of imports"

