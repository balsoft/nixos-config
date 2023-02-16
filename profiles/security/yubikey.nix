{ config, pkgs, ... }: {
	home-manager.users.balsoft = {
    home.activation.yubi = {
      data = ''
        mkdir -p .config/Yubico
        [ -f /home/balsoft/.config/Yubico/u2f_keys ] || (pamu2fcfg > /home/balsoft/.config/Yubico/u2f_keys)
      '';
      after = [ "linkGeneration" ];
      before = [ ];
    };
	};

  persist.state.directories = [ "/home/balsoft/.config/Yubico" ];

  security.pam.services = builtins.listToAttrs (builtins.map (name: {
    inherit name;
    value = { unixAuth = false; };
  }) [
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
  ]);

  security.pam.u2f = {
    control = "sufficient";
    cue = true;
    enable = true;
  };

}
