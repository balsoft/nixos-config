{ config, pkgs, lib, ... }: {
  environment.loginShellInit = lib.mkBefore ''
    [[ "$(tty)" == /dev/tty? ]] && sudo /run/current-system/sw/bin/lock this
  '';

  environment.systemPackages = [
    (pkgs.writeShellScriptBin "lock" ''
      set -euo pipefail
      if [[ "$1" == this ]]
        then args="-s"
        else args="-san"
      fi
      ${lib.optionalString (config.deviceSpecific.isLaptop)
      ''USER=balsoft ${pkgs.vlock}/bin/vlock "$args"''}
    '')
  ];

  security.sudo = {
    enable = true;
    extraConfig = ''
      balsoft ALL = (root) NOPASSWD: /run/current-system/sw/bin/lock
      balsoft ALL = (root) NOPASSWD: /run/current-system/sw/bin/lock this
      balsoft ALL = (root) NOPASSWD: ${pkgs.light}/bin/light -A 5
      balsoft ALL = (root) NOPASSWD: ${pkgs.light}/bin/light -U 5
    '';
  };
}
