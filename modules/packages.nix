{ pkgs, config, lib, inputs, ... }:
let
  filterGit =
    builtins.filterSource (type: name: name != ".git" || type != "directory");
  system = "x86_64-linux";
  old = import inputs.nixpkgs-old ({
    config = config.nixpkgs.config;
    localSystem = { inherit system; };
  });
in {
  nixpkgs.overlays = [
    inputs.nix.overlay
    (self: super:
      rec {
        nix = super.nix // {
          meta = super.nix.meta // { platforms = lib.platforms.unix; };
        };

        nur = (import inputs.NUR {
          pkgs = old;
          nurpkgs = pkgs;
        }).repos;

        inherit (nur.balsoft.pkgs) termNote lambda-launcher nix-patch;

        all-hies = import inputs.all-hies { };

        yt-utilities =
          import (self.fetchgit config.secrets.yt-utilities.source) { };

        mtxclient = super.mtxclient.overrideAttrs (oa: rec {
          name = "${pname}-${version}";
          buildInputs = with self; [
            boost170
            nlohmann_json
            openssl
            zlib
            libsodium
            olm
          ];
          pname = "mtxclient";
          cmakeFlags = oa.cmakeFlags ++ [
            "-DBoost_NO_BOOST_CMAKE=ON"
            "-DBoost_LIBRARY_DIR_RELEASE=${pkgs.boost170}"
          ];
          version = "0.3.0";
          src = inputs.mtxclient;
        });

        nheko = super.nheko.overrideAttrs (oa: rec {
          name = "${pname}-${version}";
          buildInputs = with self; [
            nlohmann_json
            qt5.qtquickcontrols2
            mtxclient
            olm
            boost170
            lmdb
            spdlog
            cmark
            qt5.qtbase
            qt5.qtmultimedia
            qt5.qttools
            qt5.qtgraphicaleffects
          ];
          cmakeFlags = oa.cmakeFlags ++ [
            "-DBUILD_SHARED_LIBS=ON"
            "-DBoost_NO_BOOST_CMAKE=ON"
            "-DBoost_LIBRARY_DIR_RELEASE=${pkgs.boost170}"
          ];
          pname = "nheko";
          version = "0.7.0";
          src = inputs.nheko;
        });

        nerdfonts = nur.balsoft.pkgs.roboto-mono-nerd;

        mpd-mpris = super.mpd-mpris.overrideAttrs
          (oa: { patches = [ ./mpd-mpris.patch ]; });

        mobile-broadband-provider-info =
          super.mobile-broadband-provider-info.overrideAttrs (oa: {
            src = inputs.mobile-broadband-provider-info;
            nativeBuildInputs = [ self.autoreconfHook ];
            buildInputs = [ self.libxslt ];
          });

        inherit (old) mautrix-telegram;

        pythonPackages = super.pythonPackages.override {
          overrides = (self: super: {
            pykka2 = super.pykka.overridePythonAttrs (oa: {
              src = inputs.pykka;
              version = "2.0.1";
              name = "pykka-2.0.1";
            });
            backports_functools_lru_cache =
              super.backports_functools_lru_cache.overrideAttrs
              (oldAttrs: oldAttrs // { meta.priority = 1000; });
          });
        };

        ebtables = old.ebtables;
      } // (if config.device == "Prestigio-Laptop" then {
        grub2 = super.pkgsi686Linux.grub2;
      } else
        { }))
    (self: super: builtins.mapAttrs (_: v: pkgs.callPackage v.override { }) {
      inherit (import inputs.nixpkgs-mopidy {
        localSystem.system = "x86_64-linux";
      })
        mopidy mopidy-gmusic mopidy-mpd mopidy-mpris mopidy-youtube pykka;
    })
  ];
  nixpkgs.config = {
    allowUnfree = true;
    android_sdk.accept_license = true;
    firefox.enablePlasmaBrowserIntegration = true;
  };
  environment.etc.nixpkgs.source = inputs.nixpkgs;
  nix = rec {
    nixPath = lib.mkForce [
      "nixpkgs=/etc/nixpkgs"
      "nixos-config=/etc/nixos/configuration.nix"
    ];
    binaryCaches = [ "https://cache.nixos.org" "https://cache.balsoft.ru" ];

    trustedUsers = [ "root" "balsoft" "@wheel" ];

    optimise.automatic = true;

    binaryCachePublicKeys =
      [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];

    package = pkgs.nixFlakes;

    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
}
