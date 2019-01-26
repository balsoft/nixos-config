{config, pkgs, lib, ...}:
{
security.apparmor.enable = true;
  users.mutableUsers = false;
  users.users.balsoft = {
    isNormalUser = true;
    extraGroups = ["sudo" "wheel" "networkmanager" "disk" "sound" "pulse" "adbusers" "input" "libvirtd" "vboxusers" "wireshark"];
    description = "Александр Бантьев";
    uid = 1000;
    password = "";
  };
  users.users.svetlana-banteva = {
    isNormalUser = true;
    extraGroups = ["pulse" "input"];
    description = "Светлана Бантьева";
    password = "";    
  };
  users.users.bigsoft = {
    isNormalUser = true;
    extraGroups = [ "pulse" "input" "vboxusers" "networkmanager" ];
    description = "Игорь Бантьев";
    password = "";    
  };
  security.sudo = {
    enable = true;
    extraConfig = ''
balsoft ALL = (root) NOPASSWD: /run/current-system/sw/bin/nixos-rebuild switch
    '';
  };
  nix.requireSignedBinaryCaches = false;  

  home-manager.users.bigsoft = if config.device == "ASUS-Laptop" then {
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
  } else {

  };
}
