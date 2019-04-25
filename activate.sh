#!/bin/sh
cd $(dirname $0)
ln -s $(readlink result) /nix/var/nix/profiles/system-$(git rev-parse HEAD)-link
./result/bin/switch-to-configuration switch
