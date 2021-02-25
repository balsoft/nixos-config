{
  description =
    "A collection of crap, hacks and copy-paste to make my localhosts boot";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    # nixpkgs-mesa.url = github:nixos/nixpkgs-channels/bdac777becdbb8780c35be4f552c9d4518fe0bdb;
    lambda-launcher.url = "github:balsoft/lambda-launcher";
    deploy-rs.url = "github:serokell/deploy-rs";
    NUR = {
      url = "github:nix-community/NUR";
      flake = false;
    };
    base16-unclaimed-schemes = {
      url = "github:chriskempson/base16-unclaimed-schemes";
      flake = false;
    };
    home-manager.url = "github:rycee/home-manager";
    materia-theme = {
      url = "github:nana-4/materia-theme";
      flake = false;
    };
    nixpkgs-old = {
      url = "github:nixos/nixpkgs/nixos-19.09";
      flake = false;
    };
    weechat-scripts = {
      url = "github:weechat/scripts";
      flake = false;
    };
    simple-nixos-mailserver = {
      url = "gitlab:simple-nixos-mailserver/nixos-mailserver";
      flake = false;
    };
    nixpkgs-wayland = {
      url = "github:colemickens/nixpkgs-wayland";
      flake = false;
    };
    weechat-notify-send = {
      url = "github:s3rvac/weechat-notify-send";
      flake = false;
    };
    yt-utilities = {
      type = "git";
      url = "ssh://git@github.com/serokell/yt-utilities";
      ref = "flake";
    };
    nixos-fhs-compat.url = "github:balsoft/nixos-fhs-compat";
    simple-osd-daemons.url = "github:balsoft/simple-osd-daemons";
    wee-slack.url = "github:wee-slack/wee-slack";
    wee-slack.flake = false;
  };

  outputs = { nixpkgs, nix, self, deploy-rs, ... }@inputs: {
    nixosModules = import ./modules;

    nixosProfiles = import ./profiles;

    nixosConfigurations = with nixpkgs.lib;
      let
        hosts = builtins.attrNames (builtins.readDir ./machines);
        mkHost = name:
          nixosSystem {
            system = builtins.readFile (./machines + "/${name}/system");
            modules = [ (import (./machines + "/${name}")) { device = name; } ];
            specialArgs = { inherit inputs; };
          };
      in genAttrs hosts mkHost;

    legacyPackages.x86_64-linux =
      (builtins.head (builtins.attrValues self.nixosConfigurations)).pkgs;

    defaultApp = deploy-rs.defaultApp;

    deploy = {
      user = "root";
      nodes.T420-Laptop = {
        hostname =
          self.nixosConfigurations.T420-Laptop.config.networking.hostName;
        profiles.system.path = deploy-rs.lib.x86_64-linux.activate.nixos
          self.nixosConfigurations.T420-Laptop;
      };
    };
  };
}
