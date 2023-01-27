{ inputs, ... }: {
  imports = with inputs.self.nixosProfiles; [
    inputs.home-manager.nixosModules.home-manager

    # PROFILES
    autoRun
    xdg

    boot
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
