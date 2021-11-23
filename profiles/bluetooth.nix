{ pkgs, lib, ... }: {
  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluezFull;
  };

  systemd.services.bluetooth.serviceConfig.ExecStart = lib.mkForce [
    ""
    "${pkgs.bluezFull}/libexec/bluetooth/bluetoothd -f /etc/bluetooth/main.conf -E"
  ];

  persist.state.directories = [ "/var/lib/bluetooth" ];
}
