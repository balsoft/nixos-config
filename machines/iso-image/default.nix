{ modulesPath, lib, inputs, ... }: {
  imports = [
    "${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix"
    inputs.self.nixosProfiles.base
  ];
  networking.wireless.enable = lib.mkForce false;
  services.openssh.permitRootLogin = lib.mkForce "no";
  services.getty.autologinUser = lib.mkForce "balsoft";
  disabledModules = [ "installer/cd-dvd/channel.nix" ];
}
