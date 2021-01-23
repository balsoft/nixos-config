{ inputs, ... }: {
  imports = with inputs.self.nixosModules; [
    inputs.home-manager.nixosModules.home-manager

    boot
    secrets
    secrets-envsubst

    locale
    network
    overlay
    devices
    nix
    security
    ssh
    git
    gpg
    zsh
    misc
  ];
}
