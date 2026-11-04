#!/bin/sh

read -p "Hostname: " hostname

echo "Welcome to your $hostname!"

set -x

mkdir -p machines/$hostname

echo -n "$(uname -m)-linux" > machines/$hostname/system

nixos-generate-config --no-filesystems --show-hardware-config > machines/$hostname/hardware-configuration.nix

if [[ ! -f machines/$hostname/default.nix ]]; then
  cat <<EOF > machines/$hostname/default.nix
  {
    inputs,
    lib,
    config,
    pkgs,
    ...
  }:
  {
    imports = [
      ./hardware-configuration.nix
      inputs.self.nixosRoles.desktop
    ];

    fileSystems."/persist" = {
      device = "/dev/nvme0n1p2";
      fsType = "ext4";
      neededForBoot = true;
    };
    fileSystems."/boot" = {
      device = "/dev/nvme0n1p1";
      fsType = "vfat";
    };

    persist = {
      enable = true;
      cache.clean.enable = true;
    };

    nix.settings.max-jobs = lib.mkDefault 16;
    hardware.enableRedistributableFirmware = true;

    security.pam.services.login.unixAuth = lib.mkForce true; # For first login, in case pamu2f thing breaks
  }
  EOF
fi

nix-shell -p helix --run "hx machines/$hostname/*"

git add machines/$hostname

nixos-install --flake .#$hostname --root /persist

mkdir -p /perist/home/balsoft/.config/Yubico

echo "balsoft:$(nix-shell -p pam_u2f --run "pamu2fcfg -n")" > /persist/home/balsoft/.config/Yubico/u2f_keys

chown -R 1000:100 /persist/home/balsoft
