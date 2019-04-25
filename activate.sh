#!/bin/sh
ln -s $(readlink $(pwd)/result) /nix/var/nix/profiles/system-$(date +%s)-link
$(pwd)/result/bin/switch-to-configuration switch
