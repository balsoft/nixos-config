{ pkgs, lib, inputs, ... }: {
  nix = rec {
    nixPath = lib.mkForce [ "self=/etc/self/compat" "nixpkgs=/etc/nixpkgs" ];
    binaryCaches = [
      "https://cache.nixos.org"
    ];

    trustedBinaryCaches = [
      "s3://serokell-private-cache?endpoint=s3.eu-central-1.wasabisys.com&profile=serokell-private-cache-wasabi"
    ];

    registry.self.flake = inputs.self;
    registry.np.flake = inputs.nixpkgs;

    trustedUsers = [ "root" "balsoft" "@wheel" ];

    nrBuildUsers = 16;

    optimise.automatic = true;

    binaryCachePublicKeys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "serokell-1:aIojg2Vxgv7MkzPJoftOO/I8HKX622sT+c0fjnZBLj0="
    ];

    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    requireSignedBinaryCaches = true;
  };

  persist.state.directories = [ "/home/balsoft/.local/share/nix" ];

  environment.etc.nixpkgs.source = inputs.nixpkgs;
  environment.etc.self.source = inputs.self;
}
