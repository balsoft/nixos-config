{ inputs, ... }: {
  imports = [
    ./base.nix
  ];

  security.sudo.wheelNeedsPassword = false;
}
