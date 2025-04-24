{
  pkgs,
  lib,
  config,
  ...
}:
{
  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluez;
  };

  systemd.services.bluetooth.serviceConfig.ExecStart = lib.mkForce [
    ""
    "${pkgs.bluez}/libexec/bluetooth/bluetoothd -f /etc/bluetooth/main.conf -E"
  ];

  persist.state.directories = [ "/var/lib/bluetooth" ];
  home-manager.users.balsoft =
    let
      headphones = "80:99:E7:8B:AE:D5";
    in
    {
      services.mpris-proxy.enable = true;
      programs.zsh.shellAliases = {
        "hpc" = "bluetoothctl connect ${headphones}";
        "hpd" = "bluetoothctl disconnect ${headphones}";
      };

      wayland.windowManager.sway.config.keybindings =
        let
          inherit (config.home-manager.users.balsoft.wayland.windowManager.sway.config)
            modifier
            ;
        in
        {
          "${modifier}+F2" = "exec bluetoothctl connect ${headphones}";
          "${modifier}+Shift+F2" = "exec bluetoothctl disconnect ${headphones}";
        };
    };
}
