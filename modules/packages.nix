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

        inherit (nur.balsoft.pkgs) termNote nix-patch;

        inherit (old) mautrix-telegram;

        inherit (inputs.lambda-launcher.packages.x86_64-linux) lambda-launcher;

        inherit old;

        yt-utilities =
          import (self.fetchgit config.secrets.yt-utilities.source) { };

        nerdfonts = nur.balsoft.pkgs.roboto-mono-nerd;



        mobile-broadband-provider-info =
          super.mobile-broadband-provider-info.overrideAttrs (oa: {
            src = inputs.mobile-broadband-provider-info;
            nativeBuildInputs = [ self.autoreconfHook ];
            buildInputs = [ self.libxslt ];
          });
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
    ];
    binaryCaches = [ "https://cache.nixos.org" "https://cache.balsoft.ru" ];

    registry.self.flake = inputs.self;

    trustedUsers = [ "root" "balsoft" "@wheel" ];

    nrBuildUsers = 16;

    optimise.automatic = true;

    binaryCachePublicKeys =
      [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];

    package = inputs.nix.packages.x86_64-linux.nix;

    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    requireSignedBinaryCaches = false;
  };
}
