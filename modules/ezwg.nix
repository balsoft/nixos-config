# Kudos to https://github.com/notgne2

{ config, lib, pkgs, ... }:
with lib;
let cfg = config.services.ezwg;
in {
  options.services.ezwg = {
    enable = mkEnableOption "Enable simple Wireguard connection";
    proxy = mkOption {
      type = types.bool;
      default = true;
      description = "Route all your traffic through this connection";
    };
    lanSize = mkOption {
      type = types.int;
      default = 24;
      description = "Size of your VLAN (only relevant if proxy is false)";
    };
    serverIP = mkOption {
      type = types.str;
      description = "The IP of the wg server";
    };
    serverPort = mkOption {
      type = types.int;
      default = 51820;
      description = "The port of the wg server";
    };
    serverKey = mkOption {
      type = types.str;
      description = "The public key of the wg server";
    };
    privateKeyFile = mkOption {
      type = types.str;
      description = "Private wg key";
    };
    vlanIP = mkOption {
      type = types.str;
      description = "The IP to use on the wg VLAN";
    };
  };
  config = mkIf cfg.enable {
    networking.firewall.checkReversePath = false;
    systemd.services.wireguard-wg0.wantedBy = lib.mkForce [ ];
    systemd.paths.wireguard-wg0.wantedBy = lib.mkForce [ ];
    systemd.services."wireguard-wg0-peer-${
      lib.replaceChars [ "/" "-" " " "+" "=" ] [
        "-"
        "\\x2d"
        "\\x20"
        "\\x2b"
        "\\x3d"
      ] cfg.serverKey
    }".wantedBy = lib.mkForce [ ];

    networking.wireguard.interfaces.wg0 = let
      generateRangesScript =
        builtins.toFile "exclusionary-wildcard-ranges-generator.py" ''
          import ipaddress
          n1 = ipaddress.ip_network('0.0.0.0/0')
          n2 = ipaddress.ip_network('${cfg.serverIP}/32')
          print(':'.join(list(map(lambda x: str(x), list(n1.address_exclude(n2))))), end="")
        '';
      rangesOutput = pkgs.runCommand "exclusionary-wildcard-ranges" { } ''
        ${pkgs.python3}/bin/python3 ${generateRangesScript} > $out
      '';
      generateSubnetScript =
        builtins.toFile "subnet-without-host-bits-generator.py" ''
          import ipaddress
          n1 = ipaddress.ip_network('${cfg.vlanIP}/${
            toString cfg.lanSize
          }', False)
          print(n1, end="")
        '';
      subnetOutput = pkgs.runCommand "subnet-without-host-bits" { } ''
        ${pkgs.python3}/bin/python3 ${generateSubnetScript} > $out
      '';
      ranges = lib.splitString ":" (builtins.readFile "${rangesOutput}");
      subnet = builtins.readFile "${subnetOutput}";
    in {
      ips = [ "${cfg.vlanIP}/${toString cfg.lanSize}" ];
      privateKeyFile = cfg.privateKeyFile;
      peers = [{
        publicKey = cfg.serverKey;
        allowedIPs = if cfg.proxy then ranges else [ subnet ];
        endpoint = "${cfg.serverIP}:${toString cfg.serverPort}";
        persistentKeepalive = 25;
      }];
    };
  };
}
