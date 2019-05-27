{ pkgs, config, lib, ... }: {
  nixpkgs.overlays = [(self: old:
  {
    termNote = self.callPackage "${self.fetchFromGitHub {
      owner = "terodom";
      repo = "termnote";
      rev = "8d96b7d3d66f725b1f395e8b8eeea82c59f3956f";
      sha256 = "07wcd0py5hxdbfl47mkdrk5mcq3w7b4v5f7caidz4ap082kmvyxh";
    }}/termNote.nix" { };

    nixfmt = self.callPackage (self.fetchFromGitHub {
      owner = "serokell";
      repo = "nixfmt";
      rev = "c1da487d3f4b62a4d5be429dfdb483920a9ea482";
      sha256 = "1g8lc7rbbg4bfgjcspwd2dv2xn9b6c1inwl5nw01zdl0r8k4s3y9";
    }) { };

    lambda-launcher = (import (self.fetchFromGitHub {
      owner = "balsoft";
      repo = "lambda-launcher";
      rev = "8d1aaa8af38382852afe92b35d525a21d0a95e54";
      sha256 = "0v2r37yj6g31fyhdhn6vwn4yzgfxkv65pg1y0iv4q6m0qlbjsikf";
    }) { pkgs = old; }).lambda-launcher;

    tdesktop = old.tdesktop.overrideAttrs (oldAttrs: {
      patches = [(old.fetchurl {
        url =
          https://raw.githubusercontent.com/msva/mva-overlay/master/net-im/telegram-desktop/files/patches/9999/conditional/wide-baloons/0001_baloons-follows-text-width-on-adaptive-layout.patch;
        sha256 =
          "46a008dcd235356427dbd717f39d41d44bceffb27be15624344f22e638802b8a";
      })] ++ oldAttrs.patches;
    });
    

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
