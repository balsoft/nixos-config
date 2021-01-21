{ pkgs, lib, config, ... }:
with lib;
with types; {
  options = {
    device = mkOption { type = str; };
    deviceSpecific = {
      isLaptop = mkOption {
        type = bool;
        default =
          !isNull (builtins.match ".*Laptop" config.networking.hostName);
      };
      devInfo = {
        cpu = {
          arch = mkOption { type = enum [ "x86_64" "aarch64" ]; };
          vendor = mkOption { type = enum [ "amd" "intel" "broadcom" ]; };
          clock = mkOption { type = int; };
          cores = mkOption { type = int; };
        };
        drive = {
          type = mkOption { type = enum [ "hdd" "ssd" ]; };
          speed = mkOption { type = int; };
          size = mkOption { type = int; };
        };
        ram = mkOption { type = int; };
        bigScreen = mkOption {
          type = bool;
          default = true;
        };
      };
      # Whether machine is powerful enough for heavy stuff
      goodMachine = with config.deviceSpecific;
        mkOption {
          type = bool;
          default = devInfo.cpu.clock * devInfo.cpu.cores >= 4000
            && devInfo.drive.size >= 100 && devInfo.ram >= 8;
        };
      isHost = mkOption {
        type = bool;
        default = with config.deviceSpecific; goodMachine;
      };
      bigScreen = mkOption {
        type = bool;
        default = config.deviceSpecific.devInfo ? bigScreen;
      };
    };
  };
}
