{ inputs, ... }: {
  imports = [
    ./base.nix

    inputs.self.nixosProfiles.boot
  ];

  security.sudo.wheelNeedsPassword = false;
}
