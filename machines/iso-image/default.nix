{ modulesPath, lib, inputs, pkgs, ... }: {
  imports = with inputs.self.nixosModules; [
    "${modulesPath}/installer/cd-dvd/installation-cd-minimal.nix"
    inputs.self.nixosRoles.base
    themes
    fonts
    cage
    gtk
    alacritty
  ];
  networking.wireless.enable = lib.mkForce false;
  services.openssh.permitRootLogin = lib.mkForce "no";
  services.getty.autologinUser = lib.mkForce "balsoft";
  boot.kernelPackages = lib.mkForce pkgs.linuxPackages_latest;
  boot.supportedFilesystems = lib.mkForce [ "ext4" "vfat" ];
  disabledModules = [ "installer/cd-dvd/channel.nix" ];
}
