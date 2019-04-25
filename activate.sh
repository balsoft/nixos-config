#!/usr/bin/env nix-shell
#!nix-shell -p coreutils bash -i bash

dir=$(dirname $0)
cd $dir
ln -s $(readlink result) /nix/var/nix/profiles/system-$(git rev-parse HEAD)-link
$dir/result/bin/switch-to-configuration switch
