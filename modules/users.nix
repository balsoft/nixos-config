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

  users.users.yuroksov = lib.mkIf (config.device == "AMD-Workstation") {
    isNormalUser = true;
    description = "Юрий Шараковский";
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC2DG3/icUe7LqtEFj1F3OaNFFEHJzxQU7zCjayZtcMejp+aDq7j+puNksbAPjtMcUHccygTni7oNibGWUZ7FFG/HrP1ETYmd99Cg13nWOabYHtkIvNrSAjReJcpyJY8CqqMIJ1p43VYa/yUahwiQ9SjVZYAfbBwrEVk5HLxsbtvDycpoGTCqygXueD7S+mVVheCZVSYKnih26XpMd0nM5kCkGv+Rqec3NBrot06JZIV3YO3/bmuxyhW4PVTh8Kfvp7ov2bhpbtbS0IZ3MtxCM5rXxE/+C0h8LyB1AsiSARDMruNjPL2vFgnrqe6PHSk5gEQwtoRulVXzyq79EUxOBG1BntsPBTUKHIaNclTXki8uJiEoH6WOhwRZrZN4dGFiq5S+9uG8hVPt+rLsyaVvBt96Wmw++YcLmVw3o8RmDggqcOgWh/6C9ONpeDDSNCYFwhD5Oj8at3JO6cwOKTCQZCelOuzrEe8ufRWjE6XNPIbBPGZee4nFUgSsMI5pwmcA8= yuroksov@Warrior"
    ];
  };

  users.users.alexey-pleshkov = lib.mkIf (config.device == "AMD-Workstation") {
    isNormalUser = true;
    description = "Алексей Плешков";
    openssh.authorizedKeys.keys = [ ];
  };

  users.users.viktory = lib.mkIf (config.device == "AMD-Workstation") {
    isNormalUser = true;
    description = "Виктория Дёмина";
    openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDVbFuOumlc5Vg43w0j2VKm0QejuY6Zv/ylo8sMYMcQRGA12QiHSCmoSn1PvcSNxk6FseBPN5ck8/rnEq9bgOAeW8uHpyixxCL3Qch0NemW3pTq2AMvoUTmYWTABByj0duIDuR1P2+VLrovlvue0eIio/Rmh3+KG4pcHksR1jWG97Z2nEoUTjO2+/bzzUPvRllGSh0C2EuGBP6jy1kqLLwThUiEzM4F6AYIsb235uS942GKG9l6wK/wFFUw6cwkazNkj9/TmSRRdab+0oGIX97HbzgE82eVbYLFquhwzS0LME2DGMjj7X0dXCBLxsJp2603gSxYQVmJNLlJTOltAGJZ viktory@DESKTOP-TU88M9R" ];
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
