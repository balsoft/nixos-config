{pkgs, config, lib, ...}:
{
nixpkgs.overlays = [ (self: old: {
    nur = pkgs.callPackage (import (builtins.fetchGit {
      url = "https://github.com/nix-community/NUR";
    })) {};
    termNote = (import (builtins.fetchGit {url = "https://github.com/Terodom/termNote"; ref = "master";}) {});

    movit = old.movit.overrideAttrs (oldAttrs: { # Currently, movit fails
      doCheck = false;
      GTEST_DIR = "${old.gtest.src}/googletest";
    });
    plasma5 = old.plasma5 // {xdg-desktop-portal-kde = old.plasma5.xdg-desktop-portal-kde.overrideAttrs (oldAttrs: {
      buildInputs = oldAttrs.buildInputs ++  [ old.cups ];
      });};
    tdesktop = old.tdesktop.overrideAttrs (oldAttrs: {
        patches = [
        (builtins.fetchurl "https://raw.githubusercontent.com/msva/mva-overlay/master/net-im/telegram-desktop/files/patches/9999/conditional/wide-baloons/0001_baloons-follows-text-width-on-adaptive-layout.patch")
        ] ++ oldAttrs.patches;
        });
     
    } // (if config.device == "Prestigio-Laptop" then {
    grub2 = old.pkgsi686Linux.grub2;
  } else {}) //
  ((import ../packages {callPackage = old.callPackage;})))]
  ;
  nixpkgs.pkgs = import ../imports/nixpkgs
  {
    config.allowUnfree = true;
    config.android_sdk.accept_license = true;
  } // config.nixpkgs.config;
  nix.nixPath = lib.mkForce
  [
  "nixpkgs=${../imports/nixpkgs}"
  "home-manager=${../imports/home-manager}"
  "nixos-config=/etc/nixos/configuration.nix"
  ];  
  
  
}
