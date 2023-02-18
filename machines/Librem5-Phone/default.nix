{ inputs, pkgs, lib, ... }: {
  imports = with inputs.self;
    with nixosProfiles; [
      ./hardware-configuration.nix

      nixosRoles.base

      inputs.nixos-hardware.nixosModules.purism-librem-5r4

      applications-setup
      bluetooth
      power
      hardware
      sound
      plasma-mobile

      nheko
      okular
      gwenview
      aerc
      helix
      angelfish
      nix
      qmlkonsole

      kdeconnect
      cursor
      fonts
      gtk
      qt

      pass-secret-service
    ];

  programs.ssh.askPassword =
    "${pkgs.plasma5Packages.ksshaskpass.out}/bin/ksshaskpass";

  users.users.balsoft.password = lib.mkForce "0";

  boot.kernelPackages = pkgs.linuxPackages_librem5;

  system.stateVersion = "23.05";
  home-manager.users.balsoft.home.stateVersion = "22.11";

  themes.fonts = {
    main.size = 10;
    serif.size = 10;
    mono.size = 10;
  };

  environment.systemPackages = [ pkgs.pure-maps ];
}
