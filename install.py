#!/usr/bin/env nix-shell
#!nix-shell -p python3 p7zip -i python3

import subprocess
import os

print("Balsoft's config is going to be installed.")

ask_for_password = input("Do you know the password? [Y/n]: ") != "n"

if ask_for_password:
    password = input("Password (going to be echoed!): ")
    output = subprocess.check_output(["7z", "e", "secret.nix.zip", "-y", '-p%s' % password])
    print("secret.nix extracted!")

install = input("Is this a new installation? [y/N]: ") == 'y'    
config = "/mnt/etc/nixos/configuration.nix"
if install:
    print("Creating", config)
    open(config, 'w').write('import %s "%s"' % (os.getcwd(), os.uname()[1]))
    print("Done")
    print("installing")
    subprocess.check_output(["nixos-generate-config", "--root", "/mnt"])
    subprocess.check_output(["nixos-install", "--root", "/mnt"])
    print("done")
else:
    subprocess.check_output(["sudo", "nixos-rebuild", "switch"])
