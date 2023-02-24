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
      # phosh

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
      shadowsocks

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
    mono.size = 11;
  };

  environment.systemPackages = [ pkgs.pure-maps pkgs.plasma5Packages.elisa ];

  environment.etc."gnss-share.conf".text = ''
    # Socket to sent NMEA location to
    socket="/var/run/gnss-share.sock"
    # Group to set as owner for the socket
    group="geoclue"

    # GPS device driver to use
    # Supported values: stm, stm_serial
    device_driver="stm"

    # Path to GPS device to use
    device_path="/dev/gnss0"

    # Baud rate for GPS serial device
    device_baud_rate=9600

    # Directory to load/store almanac and ephemeris data
    agps_directory="/var/cache/gnss-share"
  '';

  systemd.services.gnss-share = {
    script = "gnss-share";
    description = "GNSS location manager";
    path = [ pkgs.gnss-share ];
    wantedBy = [ "multi-user.target" ];
    before = [ "geoclue.service" ];
  };

  environment.etc."geoclue/geoclue.conf".text = lib.mkForce
    (lib.generators.toINI { } {
      network-nmea = {
        enable = true;
        nmea-socket = "/var/run/gnss-share.sock";
      };
      modem-gps.enable = true;
      cdma.enable = true;
      "3g".enable = true;
      agent.whitelist = "geoclue-demo-agent";
      wifi = {
        enable = true;
        url = "https://location.services.mozilla.com/v1/geolocate?key=geoclue";
      };
    });

  home-manager.users.balsoft.programs.git.signing.signByDefault =
    lib.mkForce false;
}
