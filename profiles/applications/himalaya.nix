{ config, pkgs, inputs, lib, ... }: {
  environment.systemPackages = [ pkgs.himalaya ];
}
