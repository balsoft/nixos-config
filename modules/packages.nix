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

        nur = (import imports.NUR { inherit pkgs; }).repos;

        inherit (nur.balsoft.pkgs) termNote lambda-launcher;

        nixfmt = self.callPackage imports.nixfmt { };

        inherit (import imports.niv { }) niv;

        all-hies = import imports.all-hies { };

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
  nixpkgs.pkgs = import imports.nixpkgs {
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
  environment.etc.nixpkgs.source = imports.nixpkgs;
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
      "https://cache.balsoft.ru"
    ];

    trustedBinaryCaches =
      (builtins.map (x: "http://${x}:5000") (builtins.attrNames config.devices))
      ++ binaryCaches;

    trustedUsers = [ "root" "balsoft" "antorika" "@wheel" ];

    optimise.automatic = true;

    binaryCachePublicKeys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "hydra.example.org-1:3cfw8jj8xtoKkQ2mAQxMFcEv2/fQATA/mjoUUIFxSgo="
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
      "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
    ];
  };
}
