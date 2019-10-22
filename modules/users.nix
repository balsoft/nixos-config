{ config, pkgs, lib, ... }: {
  security.apparmor.enable = true;
  programs.firejail.enable = true;
  users.mutableUsers = false;
  users.users.balsoft = {
    isNormalUser = true;
    extraGroups = [
      "sudo"
      "wheel"
      "networkmanager"
      "disk"
      "dbus"
      "audio"
      "docker"
      "sound"
      "pulse"
      "adbusers"
      "input"
      "libvirtd"
      "vboxusers"
      "wireshark"
    ];
    description = "Александр Бантьев";
    uid = 1000;
    password = "";
  };
  
  
  systemd.services."user@" = {
    serviceConfig = {
      Restart = "always";
    };
  };

  home-manager.users.balsoft.home.sessionVariables.XDG_RUNTIME_DIR = "/run/user/1000";

  security.sudo = {
    enable = true;
    extraConfig = ''
      balsoft ALL = (root) NOPASSWD: /run/current-system/sw/bin/nixos-rebuild switch
      balsoft ALL = (root) NOPASSWD: ${pkgs.light}/bin/light -A 5
      balsoft ALL = (root) NOPASSWD: ${pkgs.light}/bin/light -U 5
    '';
  };
  nix.requireSignedBinaryCaches = false;
  home-manager.useUserPackages = true;
}
