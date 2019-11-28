let
  imports = import ../nix/sources.nix;
  new = import imports.nixpkgs-unstable { config.allowUnfree = true; };
  filterGit =
    builtins.filterSource (type: name: name != ".git" || type != "directory");
in { pkgs, config, lib, ... }: {
  nixpkgs.overlays = [
    (self: old:
      rec {
        inherit imports;

        unstable = new;

        nur = (import imports.NUR { pkgs = import imports.nixpkgs-old {}; }).repos;

        inherit (nur.balsoft.pkgs) termNote lambda-launcher nix-patch;

        nixfmt = self.callPackage imports.nixfmt { };

        inherit (import imports.niv { }) niv;

        all-hies = import imports.all-hies { };

        yt-utilities = import (self.fetchgit config.secrets.yt-utilities.source) {};

        mtxclient = old.mtxclient.overrideAttrs (oa: rec {
          name = "${pname}-${version}";
          buildInputs = oa.buildInputs ++ [ old.nlohmann_json ];
          pname = "mtxclient";
          version = "0.3.0";
          src = imports.mtxclient;
        });

        nheko = old.nheko.overrideAttrs (oa: rec {
          name = "${pname}-${version}";
          buildInputs = oa.buildInputs ++ [ old.nlohmann_json ];
          pname = "nheko";
          version = "0.7.0";
          src = imports.nheko;
        });

        sway = (new.sway.override { wlroots = wlroots'; }).overrideAttrs
          (oa: rec {
            name = "${pname}-${version}";
            pname = "sway";
            version = "1.2";
            patches = [ ];
            src = imports.sway;
          });

        wlroots' = new.wlroots.overrideAttrs (oa: rec {
          name = "${pname}-${version}";
          outputs = [ "out" ];
          postFixup = "true";
          postInstall = "true";
          pname = "wlroots";
          version = "0.6.0";
          src = imports.wlroots;
        });

        wl-clipboard = new.wl-clipboard.overrideAttrs (oa: rec {
          name = "${pname}-${version}";
          pname = "wl-clipboard";
          version = "1.0";
          src = imports.wl-clipboard;
        });

        inherit (new) kanshi mautrix-whatsapp;

        nerdfonts = nur.balsoft.pkgs.roboto-mono-nerd;

        mopidy = old.mopidy.overridePythonAttrs (oa: {
          src = imports.mopidy;
          propagatedBuildInputs = with self.pythonPackages; [ gst-python pygobject3 pykka2 tornado_4 requests setuptools dbus-python ];
        });
        
        mopidy-youtube = old.mopidy-youtube.overridePythonAttrs (oa: {
          propagatedBuildInputs = oa.propagatedBuildInputs ++ (with self.pythonPackages; [cachetools requests-cache ]);
          src = imports.mopidy-youtube;
        });

        pythonPackages = old.pythonPackages.override {
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

      } // (if config.device == "Prestigio-Laptop" then {
        grub2 = old.pkgsi686Linux.grub2;
      } else
        { }))
  ];
  nixpkgs.pkgs = import imports.nixpkgs {
    config.allowUnfree = true;
    config.android_sdk.accept_license = true;
    config.firefox.enablePlasmaBrowserIntegration = true;
  } // config.nixpkgs.config;

  systemd.services.setup_root = {
    serviceConfig.User = "root";
    script = ''
      mkdir -p /root/.ssh
      cat << EOF > /root/.ssh/id_rsa
      ${config.secrets.id_rsa}
      EOF
      chmod 100 /root/.ssh/id_rsa
    '';
  };
  environment.etc.nixpkgs.source = imports.nixpkgs;
  nix = rec {
    nixPath = lib.mkForce [
      "nixpkgs=/etc/nixpkgs"
      "nixos-config=/etc/nixos/configuration.nix"
    ];
    binaryCaches = [
      "https://cache.nixos.org"
      "http://hydra.typeable.io:5000"
      "https://all-hies.cachix.org"
      "https://cache.balsoft.ru"
    ];

    trustedUsers = [ "root" "balsoft" "@wheel" ];

    optimise.automatic = true;

    binaryCachePublicKeys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hydra.example.org-1:3cfw8jj8xtoKkQ2mAQxMFcEv2/fQATA/mjoUUIFxSgo="
      "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
    ];
  };
}
