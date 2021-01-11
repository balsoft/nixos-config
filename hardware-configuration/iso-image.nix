{ modulesPath, lib, ... }: {
  imports = [
    "${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix"
  ];
  networking.wireless.enable = lib.mkForce false;
  services.openssh.permitRootLogin = lib.mkForce "no";
  services.mingetty.autologinUser = lib.mkForce "balsoft";
  disabledModules = [ "installer/cd-dvd/channel.nix" ];
}
