{ pkgs, lib, config, ... }: {
  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluezFull;
  };

  systemd.services.bluetooth.serviceConfig.ExecStart = lib.mkForce [
    ""
    "${pkgs.bluezFull}/libexec/bluetooth/bluetoothd -f /etc/bluetooth/main.conf -E"
  ];

  persist.state.directories = [ "/var/lib/bluetooth" ];
  home-manager.users.balsoft = let headphones = "CC:98:8B:C0:FC:D2";
  in {
    programs.zsh.shellAliases = {
      "hpc" = "bluetoothctl connect ${headphones}";
      "hpd" = "bluetoothctl disconnect ${headphones}";
    };

    wayland.windowManager.sway.config.keybindings = let
      inherit (config.home-manager.users.balsoft.wayland.windowManager.sway.config)
        modifier;
    in {
      "${modifier}+F2" = "exec bluetoothctl connect ${headphones}";
      "${modifier}+Shift+F2" = "exec bluetoothctl disconnect ${headphones}";
    };
  };
}
