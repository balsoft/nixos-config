let
  imports = import ../nix/sources.nix;
  new = import imports.nixpkgs-unstable { config.allowUnfree = true; };
  filterGit =
    builtins.filterSource (type: name: name != ".git" || type != "directory");
  old = import imports.nixpkgs-old { };
in { pkgs, config, lib, ... }: {
  nixpkgs.overlays = [
    (self: super:
      rec {
        inherit imports;

        unstable = new;

        nur = (import imports.NUR { pkgs = old; }).repos;

        inherit (nur.balsoft.pkgs) termNote lambda-launcher nix-patch;

        nixfmt = self.callPackage imports.nixfmt { };

        inherit (import imports.niv { }) niv;

        all-hies = import imports.all-hies { };

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
          src = imports.mtxclient;
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
          src = imports.nheko;
        });

        sway-unwrapped = (new.sway-unwrapped.override { wlroots = wlroots'; }).overrideAttrs
          (oa: rec {
            name = "${pname}-${version}";
            pname = "sway";
            version = "master";
            patches = [ ];
            src = imports.sway;
          });

        wlroots' = new.wlroots.overrideAttrs (oa: rec {
          name = "${pname}-${version}";
          outputs = [ "out" ];
          postFixup = "true";
          postInstall = "true";
          pname = "wlroots";
          patches = [ ];
          version = "master";
          src = imports.wlroots;
        });

        wl-clipboard = new.wl-clipboard.overrideAttrs (oa: rec {
          name = "${pname}-${version}";
          pname = "wl-clipboard";
          version = "master";
          src = imports.wl-clipboard;
        });

        inherit (new) kanshi mautrix-whatsapp;

        nerdfonts = nur.balsoft.pkgs.roboto-mono-nerd;

        mopidy = super.mopidy.overridePythonAttrs (oa: {
          src = imports.mopidy;
          propagatedBuildInputs = with self.pythonPackages; [
            gst-python
            pygobject3
            pykka2
            tornado_4
            requests
            setuptools
            dbus-python
            protobuf
          ];
        });

        mopidy-youtube = super.mopidy-youtube.overridePythonAttrs (oa: {
          propagatedBuildInputs = oa.propagatedBuildInputs
            ++ (with self.pythonPackages; [ cachetools requests-cache ]);
          src = imports.mopidy-youtube;
        });
        mopidy-gmusic = super.mopidy-gmusic.overridePythonAttrs (oa: {
          propagatedBuildInputs = oa.propagatedBuildInputs ++ [ self.pythonPackages.protobuf ];
        });
        mpd-mpris = super.mpd-mpris.overrideAttrs (oa: {
          patches = [ ./mpd-mpris.patch ];
        });

        mautrix-telegram = old.mautrix-telegram;

        pythonPackages = super.pythonPackages.override {
          overrides = (self: super: {
            pykka2 = super.pykka.overridePythonAttrs (oa: {
              src = imports.pykka;
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
  ];
  nixpkgs.pkgs = import imports.nixpkgs {
    config.allowUnfree = true;
    config.android_sdk.accept_license = true;
    config.firefox.enablePlasmaBrowserIntegration = true;
  } // config.nixpkgs.config;

  environment.etc.nixpkgs.source = imports.nixpkgs;
  nix = rec {
    nixPath = lib.mkForce [
      "nixpkgs=/etc/nixpkgs"
      "nixos-config=/etc/nixos/configuration.nix"
    ];
    binaryCaches = [
      "https://cache.nixos.org"
      "https://all-hies.cachix.org"
      "https://cache.balsoft.ru"
    ];

    trustedUsers = [ "root" "balsoft" "@wheel" ];

    optimise.automatic = true;

    binaryCachePublicKeys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
    ];
  };
}
