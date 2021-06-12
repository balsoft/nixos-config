{ lib, inputs, ... }: {
  nix = rec {
    nixPath = lib.mkForce [ "self=/etc/self/compat" "nixpkgs=/etc/nixpkgs" ];
    binaryCaches = [ "https://cache.nixos.org" ];

    registry.self.flake = inputs.self;
    registry.nixpkgs.flake = inputs.nixpkgs;

    trustedUsers = [ "root" "balsoft" "@wheel" ];

    nrBuildUsers = 16;

    optimise.automatic = true;

    binaryCachePublicKeys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    ];

    package = inputs.nix.packages.x86_64-linux.nix;

    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    requireSignedBinaryCaches = true;
  };

  persist.state.homeFiles = [ ".local/share/nix/repl-history" ];

  environment.etc.nixpkgs.source = inputs.nixpkgs;
  environment.etc.self.source = inputs.self;
}
