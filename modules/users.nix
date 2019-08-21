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
  users.users.svetlana-banteva = lib.mkIf config.deviceSpecific.isShared {
    isNormalUser = true;
    extraGroups =
      [ "pulse" "input" "sound" "audio" "video" "networkmanager" "disk" ];
    packages = with pkgs; [ kdeconnect libreoffice krita gwenview okular kate ];
    description = "Светлана Бантьева";
    password = "";
  };
  users.users.bigsoft = lib.mkIf config.deviceSpecific.isShared {
    isNormalUser = true;
    extraGroups = [ "pulse" "input" "vboxusers" "networkmanager" ];
    description = "Игорь Бантьев";
    password = "";
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

  users.users.antorika = lib.mkIf (config.device == "AMD-Workstation") {
    isNormalUser = true;
    createHome = false;
    useDefaultShell = true;
    description = "Hydra builder";
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB4IrtzPTrOmWjvS4WoHT0nuHk30pWY5uZQic6vXlH9o"
    ];
  };

  home-manager.users.bigsoft = lib.mkIf config.deviceSpecific.isShared {
    xsession = {
      enable = true;
      windowManager.command = ''
        MACHINE="Windows"
        VBoxManage startvm $MACHINE
        until $(VBoxManage showvminfo --machinereadable $MACHINE | grep -q ^VMState=.poweroff.)
        do
        sleep 1
        done
      '';
    };
  };
  home-manager.useUserPackages = true;
}
