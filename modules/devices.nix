{ pkgs, lib, config, ... }:
with lib;
with types; {
  options = {
    device = mkOption { type = strMatching "[A-z]*-[A-z]*"; };
    devices = mkOption { type = attrs; };
    deviceSpecific = mkOption { type = attrs; };
  };
  config = {
    deviceSpecific = let
      device = config.device;
      devInfo = config.devices.${config.device};
    in rec {
      isLaptop = (!isNull (builtins.match ".*Laptop" device));
      smallScreen = (device == "Prestigio-Laptop");
      isShared = device == "Lenovo-Workstation";
      cpu = devInfo.cpu.vendor;
      isSSD = devInfo.drive.type == "ssd";
      goodMachine = devInfo.cpu.clock * devInfo.cpu.cores >= 4000
      && devInfo.drive.size >= 100 && devInfo.ram
      >= 8; # Whether machine is powerful enough for heavy stuff
      isHost = isSSD;
    };

    devices = {
      ASUS-Laptop = {
        cpu = {
          vendor = "intel";

          clock = 2300;
          cores = 2;
        };
        drive = {
          type = "ssd";
          speed = 500;
          size = 250;
        };
        ram = 12;
      };
      HP-Laptop = {
        cpu = {
          vendor = "amd";
          clock = 3500;
          cores = 6;
        };
        drive = {
          type = "ssd";
          speed = 500;
          size = 500;
        };
        ram = 8;
      };
      Lenovo-Workstation = {
        cpu = {
          vendor = "intel";
          clock = 2500;
          cores = 2;
        };
        drive = {
          type = "ssd";
          speed = 250;
          size = 120;
        };
        ram = 8;
      };
      AMD-Workstation = {
        cpu = {
          vendor = "amd";
          clock = 4200;
          cores = 8;
        };
        drive = {
          type = "ssd";
          speed = 6000;
          size = 250;
        };
        ram = 16;
      };
      Prestigio-Laptop = {
        cpu = {
          vendor = "intel";
          clock = 1400;
          cores = 2;
        };
        drive = {
          type = "flash";
          speed = 100;
          size = 32;
        };
        ram = 2;
      };
      NixOS-VM = {
        cpu = {
          vendor = "intel";
          clock = 1000;
          cores = 1;
        };
        drive = {
          type = "ssd";
          speed = 50;
          size = 1;
        };
        ram = 4;
      };
    };
  };
}
