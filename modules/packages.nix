let 
  new = import ../imports/nixpkgs-unstable { };
  filterGit = builtins.filterSource (type: name: name != ".git" || type != "directory");
in
{ pkgs, config, lib, ... }: {
  nixpkgs.overlays = [
    (self: old:
    {
      inherit new;

      termNote =
        self.callPackage "${filterGit ../imports/github/terodom/termNote}/termNote.nix" { };

      nixfmt = self.callPackage (filterGit ../imports/github/serokell/nixfmt) { };

      lambda-launcher = (import (filterGit ../imports/github/balsoft/lambda-launcher) {
        pkgs = old;
      }).lambda-launcher;

      all-hies = import (filterGit ../imports/github/Infinisil/all-hies) { };

      mtxclient = new.mtxclient.overrideAttrs (_: rec {
        name = "${pname}-${version}";
        pname = "mtxclient";
        version = "0.3.0";
        src = filterGit ../imports/github/nheko-reborn/mtxclient;
      });
      nheko = new.nheko.overrideAttrs (_: rec {
        name = "${pname}-${version}";
        pname = "nheko";
        version = "0.7.0";
        src = filterGit ../imports/github/nheko-reborn/nheko;
      });
      
      sway = new.sway;
      
      kanshi = new.kanshi;

      nerdfonts = old.stdenv.mkDerivation rec {
        name = "RobotoMonoNerd";
        src = old.fetchzip {
          url =
            "https://github.com/ryanoasis/nerd-fonts/releases/download/v2.0.0/RobotoMono.zip";
          sha256 =
            "sha256:1i78fn62x0337p2974dn1nga1pbdi7mqg203h81yi9b79pyxv9bh";
          stripRoot = false;
        };
        installPhase = "mkdir -p $out/share/fonts; cp $src/* $out/share/fonts";
      };

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
      { }))
  ];
  nixpkgs.pkgs = import ../imports/nixpkgs {
    config.allowUnfree = true;
    config.android_sdk.accept_license = true;
    config.firefox.enablePlasmaBrowserIntegration = true;
  } // config.nixpkgs.config;

  systemd.services.setup_root = {
    serviceConfig.User = "root";
    script = ''
      cat << EOF > /root/id_rsa
      ${config.secrets.id_rsa}
      EOF
      chmod 100 /root/id_rsa
    '';
  };
  environment.etc.nixpkgs.source = ../imports/nixpkgs;
  nix = rec {
    nixPath = lib.mkForce [
      "nixpkgs=/etc/nixpkgs"
      "nixos-config=/etc/nixos/configuration.nix"
    ];
    binaryCaches = [
      "https://cache.nixos.org"
      "http://hydra.typeable.io:5000"
      "https://nixcache.reflex-frp.org"
      "https://all-hies.cachix.org"
      "https://balsoft.ru:5443"
    ];

    trustedBinaryCaches =
      (builtins.map (x: "http://${x}:5000") (builtins.attrNames config.devices))
      ++ binaryCaches;

    trustedUsers = [ "root" "balsoft" "@wheel" ];

    optimise.automatic = true;

    binaryCachePublicKeys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hydra.example.org-1:3cfw8jj8xtoKkQ2mAQxMFcEv2/fQATA/mjoUUIFxSgo="
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
      "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
    ];
  };
}
