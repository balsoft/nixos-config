{ inputs, ... }: {
  imports = with inputs.self.nixosModules; with inputs.self.nixosProfiles; [
    inputs.home-manager.nixosModules.home-manager

    # MODULES
    applications
    secrets
    secrets-envsubst
    persist


    # PROFILES
    autoRun
    xdg

    boot
    devices
    git
    gpg
    locale
    misc
    network
    nix
    security
    ssh
    zsh
  ];
}
