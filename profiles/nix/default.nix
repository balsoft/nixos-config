{ pkgs, lib, inputs, ... }: {
  nix = rec {
    nixPath = lib.mkForce [ "self=/etc/self/compat" "nixpkgs=/etc/nixpkgs" ];
    binaryCaches = [ "https://cache.nixos.org" ];

    registry.self.flake = inputs.self;
    registry.np.flake = inputs.nixpkgs;

    trustedUsers = [ "root" "balsoft" "@wheel" ];

    nrBuildUsers = 16;

    optimise.automatic = true;

    binaryCachePublicKeys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    ];

    package = inputs.nix.defaultPackage.x86_64-linux.overrideAttrs (oa: {
      patches = [./nix.patch] ++ oa.patches or [];
      # HAHA
      doInstallCheck = false;
    });

    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    requireSignedBinaryCaches = true;
  };

  persist.state.directories = [ "/home/balsoft/.local/share/nix" ];

  environment.etc.nixpkgs.source = inputs.nixpkgs;
  environment.etc.self.source = inputs.self;
}
