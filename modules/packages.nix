{pkgs, config, lib, ...}:
{
  nixpkgs.overlays = [ (self: old: {
    termNote = (import (builtins.fetchGit {url = "https://github.com/Terodom/termNote"; rev = "a4045a75dca67891ef239a43f364ce3149a91b6a";}) {});

    lambda-launcher = (import (builtins.fetchGit { url = "https://github.com/balsoft/lambda-launcher/"; rev = "275e95a26b4b4e65ac53e58c6408ca2c1675c457";}) {});    
    oldPackages = import (fetchTarball { url = https://github.com/nixos/nixpkgs-channels/archive/2aa505b77c04bfd8bb702cbf261f1141a3fe55c2.tar.gz; sha256 = "sha256:14lp7lcwfy6zs2xlhmc71dbq767inz6rp9v2wcma3lf3jf46acvl"; }) {} ;

    tdesktop = old.tdesktop.overrideAttrs (oldAttrs: {
      patches = [
        (builtins.fetchurl 
          {
            url = "https://raw.githubusercontent.com/msva/mva-overlay/master/net-im/telegram-desktop/files/patches/9999/conditional/wide-baloons/0001_baloons-follows-text-width-on-adaptive-layout.patch"; 
            sha256 = "423e9ead98358a92ed1970713b39be83c17159a1609074648164e31ba56b1b47";
          })
      ] ++ oldAttrs.patches;
    });
    pythonPackages = old.pythonPackages.override 
      {
        overrides = 
          (self: super:
            {
              backports_functools_lru_cache = super.backports_functools_lru_cache.overrideAttrs (oldAttrs:
                oldAttrs // {
                  meta.priority = 1000;
                });
            });
      };
  } // (if config.device == "Prestigio-Laptop" then {
    grub2 = old.pkgsi686Linux.grub2;
  } else {}))];
  nixpkgs.pkgs = import ../imports/nixpkgs
    {
      config.allowUnfree = true;
      config.android_sdk.accept_license = true;
      config.firefox.enablePlasmaBrowserIntegration = true;
    } // config.nixpkgs.config;


  nix = {
    nixPath = lib.mkForce
    [
      "nixpkgs=${../imports/nixpkgs}"
      "home-manager=${../imports/home-manager}"
      "nixos-config=/etc/nixos/configuration.nix"
    ];
    binaryCaches = [
      https://cache.nixos.org http://hydra.typeable.io:5000
    ];
    binaryCachePublicKeys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" 
      "hydra.example.org-1:3cfw8jj8xtoKkQ2mAQxMFcEv2/fQATA/mjoUUIFxSgo="
    ];
  };
}
