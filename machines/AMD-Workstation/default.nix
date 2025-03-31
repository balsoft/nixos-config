{ config, inputs, pkgs, ... }: {
  imports = [
    ./hardware-configuration.nix
    inputs.self.nixosRoles.desktop
    inputs.self.nixosProfiles.print-scan
    inputs.self.nixosProfiles.aws
  ];
  deviceSpecific.devInfo = {
    cpu = {
      vendor = "amd";
      clock = 4200;
      cores = 8;
    };
    drive = {
      type = "ssd";
      speed = 6000;
      size = 250;
    };
    bigScreen = true;
    ram = 32;
  };
  deviceSpecific.isHost = true;
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  # services.apcupsd = {
  #   enable = true;
  #   configText = ''
  #     UPSTYPE usb
  #     NISIP 127.0.0.1
  #     BATTERYLEVEL 10
  #     MINUTES 1
  #   '';
  # };

  secrets.wireguard-wg0 = { };

  environment.sessionVariables.WINEPREFIX =
    "/home/balsoft/.local/share/wineprefixes/default";

  services.bt-agent = {
    enable = true;
    capability = "NoInputNoOutput";
  };

  secrets.restic-backup-password = { };
  secrets.restic-backup-environment = { };

  services.restic.backups.b2 = {
    inhibitsSleep = true;
    passwordFile = config.secrets.restic-backup-password.decrypted;
    environmentFile = config.secrets.restic-backup-environment.decrypted;
    paths = [
      "/home/balsoft/Documents" # Documents, typically not in git
      "/etc/NetworkManager/system-connections" # NetworkManager's wifi networks and stuff
    ];
    # Find all projects with uncommited changes
    dynamicFilesFrom = ''
      ${pkgs.git}/bin/git config set --global safe.directory '*' # Git dirs owned by another users, should be fine
      for gitdir in $(${pkgs.findutils}/bin/find /home/balsoft/projects -name .git -type d -prune); do
        dir="$(dirname "$gitdir")";
        if \
          [[ -n "$(${pkgs.git}/bin/git -C "$dir" status  --untracked-files=no --porcelain)" ]] \
          || [[ -n "$(${pkgs.git}/bin/git -C "$dir" log --format=short --branches --not --remotes)" ]];
          then printf "%s\n" "$dir";
        fi;
      done;
      ${pkgs.git}/bin/git config unset --global safe.directory
    '';
    repository = "s3:s3.eu-central-003.backblazeb2.com/balsoft-backups";
    pruneOpts = [ "--keep-daily 7" "--keep-weekly 3" "--keep-monthly 3" ];
    # Typically I'm done writing stuff by then, save the day's work
    timerConfig = {
      OnCalendar = "22:00";
      Persistent = true;
    };
  };

  systemd.services.restic-backups-b2.wants = [
    "restic-backup-password-secrets.service"
    "restic-backup-environment-secrets.service"
  ];
  systemd.services.restic-backups-b2.requires = [
    "restic-backup-password-secrets.service"
    "restic-backup-environment-secrets.service"
  ];

  hardware.bluetooth.input.General.UserspaceHID = true;

  persist = {
    enable = true;
    cache.clean.enable = false; # Scary...

    state.directories = [ "/home/balsoft/.local/share/Steam" ];

    derivative.directories =
      [ "/home/balsoft/.local/share/wineprefixes/default" ];
  };
}
