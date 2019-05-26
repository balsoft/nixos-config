{ pkgs, config, lib, ... }: {
  nixpkgs.overlays = [(self: old:
  {
    termNote = (import (builtins.fetchGit {
      url = https://github.com/Terodom/termNote;
      rev = "a4045a75dca67891ef239a43f364ce3149a91b6a";
    }) { });

    lambda-launcher = (import (builtins.fetchGit {
      url = https://github.com/balsoft/lambda-launcher/;
      rev = "a64e2d79802353b1241570944808800828c376f0";
    }) { nixpkgs = self; });

    /*tdesktop = old.tdesktop.overrideAttrs (oldAttrs: {
      patches = ["${builtins.fetchGit {
        url =
          https://github.com/msva/mva-overlay;
        rev =
          "e5121619c9814b36284146dbe3dae92cf41a7c25";
      }}/net-im/telegram-desktop/files/patches/9999/conditional/wide-baloons/0001_baloons-follows-text-width-on-adaptive-layout.patch"] ++ oldAttrs.patches;
    });*/
    pythonPackages = old.pythonPackages.override {
      overrides = (self: super: {
        backports_functools_lru_cache =
        super.backports_functools_lru_cache.overrideAttrs
        (oldAttrs: oldAttrs // { meta.priority = 1000; });
      });
    };
  } // (if config.device == "Prestigio-Laptop" then {
    grub2 = old.pkgsi686Linux.grub2;
  } else
    { }))];
  nixpkgs.pkgs = import ../imports/nixpkgs {
    config.allowUnfree = true;
    config.android_sdk.accept_license = true;
    config.firefox.enablePlasmaBrowserIntegration = true;
  } // config.nixpkgs.config;

  nix = {
    nixPath = lib.mkForce [
      "nixpkgs=${../imports/nixpkgs}"
      "home-manager=${../imports/home-manager}"
      "nixos-config=/etc/nixos/configuration.nix"
    ];
    binaryCaches =
    [ "https://cache.nixos.org" "http://hydra.typeable.io:5000" ];
    binaryCachePublicKeys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hydra.example.org-1:3cfw8jj8xtoKkQ2mAQxMFcEv2/fQATA/mjoUUIFxSgo="
    ];
  };
}
