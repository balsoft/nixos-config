{ inputs, ... }: {
  imports = with inputs.self.nixosModules; [
    inputs.home-manager.nixosModules.home-manager

    boot
    secrets
    secrets-envsubst

    devices
    git
    gpg
    locale
    misc
    network
    nix
    overlay
    persist
    security
    ssh
    zsh
  ];
}
