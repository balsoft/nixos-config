{ config, ... }: {
  virtualisation.docker.enable = config.deviceSpecific.isHost;
  virtualisation.libvirtd = {
    enable = config.deviceSpecific.isHost;
  };

  persist.state.directories = [
    "/var/lib/docker"
    "/var/lib/libvirt"
  ];

  virtualisation.spiceUSBRedirection.enable = true;

  networking.nat = {
    enable = true;
    internalInterfaces = ["ve-+"];
  };
}
