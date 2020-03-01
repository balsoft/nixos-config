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
  
  home-manager.users.balsoft.xdg.configFile."Yubico/u2f_keys".text = "balsoft:CicONGIHB4jL1NuHA36oPlPn5qsAQgIjQKGTSC3F-XZEAWdRVBX0jr8I_PxbgQR6Ys1toFJhaXtqYjY__uVhgg,04f580832926c646e918123bd516851ac5bf7d54482c1e2eb05e0e3f6aae197e4fc4853a2760794cf597001498f1833b3d36015cafa5f61703f3fbf59b73e05eca";

  home-manager.users.balsoft.home.sessionVariables.XDG_RUNTIME_DIR = "/run/user/1000";

  security.pam.u2f = {
    control = "required";
    cue = true;
    enable = true;
  };
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
