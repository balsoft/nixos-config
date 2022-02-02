{ pkgs, lib, inputs, config, ... }: {
  nix = rec {
    nixPath = lib.mkForce [ "self=/etc/self/compat" "nixpkgs=/etc/nixpkgs" ];
    registry.self.flake = inputs.self;
    registry.np.flake = inputs.nixpkgs;

    nrBuildUsers = config.nix.settings.max-jobs;

    optimise.automatic = true;

    extraOptions = ''
      experimental-features = nix-command flakes
    '';

    settings = {
      trusted-users = [ "root" "balsoft" "@wheel" ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "serokell-1:aIojg2Vxgv7MkzPJoftOO/I8HKX622sT+c0fjnZBLj0="
      ];
      substituters = [ "https://cache.nixos.org" ];
      trusted-substituters = [
        "s3://serokell-private-cache?endpoint=s3.eu-central-1.wasabisys.com&profile=serokell-private-cache-wasabi"
      ];
    };
  };

  persist.state.directories = [ "/home/balsoft/.local/share/nix" ];

  environment.etc.nixpkgs.source = inputs.nixpkgs;
  environment.etc.self.source = inputs.self;
}
