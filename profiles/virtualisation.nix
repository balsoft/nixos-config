{ config, ... }: {
  virtualisation.docker.enable = config.deviceSpecific.isHost;
  virtualisation.libvirtd = {
    enable = config.deviceSpecific.isHost;
  };
  virtualisation.spiceUSBRedirection.enable = true;

  networking.nat = {
    enable = true;
    internalInterfaces = ["ve-+"];
  };
}
