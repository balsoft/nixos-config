{ inputs, ... }: {
  imports = with inputs.self.nixosModules; [
    inputs.home-manager.nixosModules.home-manager

    applications
    boot
    secrets
    secrets-envsubst

    autoRun
    xdg

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
