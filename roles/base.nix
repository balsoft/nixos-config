{ inputs, ... }: {
  imports = with inputs.self.nixosProfiles; [
    # PROFILES
    autoRun
    xdg

    git
    gpg
    locale
    misc
    network
    nix
    user
    ssh
    zsh
  ];
}
