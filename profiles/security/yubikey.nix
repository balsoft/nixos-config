{ config, pkgs, ... }: {
  systemd.services.pam-u2f-setup = {
    description = "Initial setup for pam_u2f - trust the key that is currently plugged in";
    wantedBy = [ "multi-user.target" ];
    requires = [ "systemd-tmpfiles-setup.service" ]; # Make sure the home directory exists already
    before = [ "getty-pre.target" ];
    wants = [ "getty-pre.target" ];
    script = ''
      [ -f /home/balsoft/.config/Yubico/u2f_keys ] || (pamu2fcfg > /home/balsoft/.config/Yubico/u2f_keys)
    '';

    path = [ pkgs.pam_u2f ];

    serviceConfig = {
      User = "balsoft";
      Type = "oneshot";
    };
  };
  persist.state.directories = [ "/home/balsoft/.config/Yubico" ];

  security.pam.services = builtins.listToAttrs (
    builtins.map
      (name: {
        inherit name;
        value = {
          unixAuth = false;
        };
      })
      [
        "chpasswd"
        "chsh"
        "groupadd"
        "groupdel"
        "groupmems"
        "groupmod"
        "i3lock"
        "i3lock-color"
        "login"
        "passwd"
        "polkit-1"
        "runuser"
        "runuser-l"
        "su"
        "sudo"
        "swaylock"
        "systemd-user"
        "useradd"
        "userdel"
        "usermod"
        "vlock"
        "xlock"
        "xscreensaver"
      ]
  );

  security.pam.u2f = {
    control = "sufficient";
    settings.cue = true;
    enable = true;
  };

}
