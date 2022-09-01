{ pkgs, lib, inputs, config, ... }: {
  nix = rec {
    nixPath = lib.mkForce [ "self=/etc/self/compat" "nixpkgs=/etc/nixpkgs" ];
    registry.self.flake = inputs.self;
    registry.np.flake = inputs.nixpkgs;

    nrBuildUsers = config.nix.settings.max-jobs;

    optimise.automatic = true;

    extraOptions = ''
      builders-use-substitutes = true
      # Enable flakes
      experimental-features = nix-command flakes
      # Prevent Nix from fetching the registry every time
      flake-registry = ${inputs.flake-registry}/flake-registry.json
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
    buildMachines = [
      # tweag remote builder
      {
        hostName = "build01.tweag.io";
        maxJobs = 24;
        sshUser = "nix";
        sshKey = "/root/.ssh/id_ed25519";
        system = "x86_64-linux";
        supportedFeatures = [ "benchmark" "big-parallel" "kvm" ];
      }
      {
        hostName = "build02.tweag.io";
        maxJobs = 24;
        sshUser = "nix";
        sshKey = "/root/.ssh/id_ed25519";
        systems = [ "aarch64-darwin" "x86_64-darwin" ];
      }
    ];
  };

  persist.state.directories = [ "/home/balsoft/.local/share/nix" ];

  environment.etc.nixpkgs.source = inputs.nixpkgs;
  environment.etc.self.source = inputs.self;
}
