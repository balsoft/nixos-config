{pkgs, config, lib, ...}:
{
  nixpkgs.config.packageOverrides = old: {
    nur = pkgs.callPackage (import (builtins.fetchGit {
      url = "https://github.com/nix-community/NUR";
    })) {};
    termNote = (import (builtins.fetchGit {url = "https://github.com/Terodom/termNote"; ref = "master";}));

    movit = old.movit.overrideAttrs (oldAttrs: { # Currently, movit fails
      doCheck = false;
      GTEST_DIR = "${old.gtest.src}/googletest";
    });
    plasma5 = old.plasma5 // {xdg-desktop-portal-kde = old.plasma5.xdg-desktop-portal-kde.overrideAttrs (oldAttrs: {
      buildInputs = oldAttrs.buildInputs ++  [ old.cups ];
      });};
    tdesktop = old.tdesktop.overrideAttrs (oldAttrs: {
        patches = [
        (builtins.fetchurl { url = "https://raw.githubusercontent.com/msva/mva-overlay/master/net-im/telegram-desktop/files/patches/1.5.6/conditional/wide-baloons/0001_baloons-follows-text-width-on-adaptive-layout.patch"; sha256 = "95800293734d894c65059421d7947b3666e3cbe73ce0bd80d357b2c9ebf5b2e5"; })
        ] ++ oldAttrs.patches;
        });
     
    } // (if config.device == "Prestigio-Laptop" then {
    grub2 = old.pkgsi686Linux.grub2;
  } else {}) //
  ((import ../packages {callPackage = old.callPackage;}))
  ;
  nixpkgs.config.android_sdk.accept_license = true;
  
  nixpkgs.config.allowUnfree = true;
}
