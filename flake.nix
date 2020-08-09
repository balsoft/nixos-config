{
  description =
    "A collection of crap, hacks and copy-paste to make my localhosts boot";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/c253216595572930316f2be737dc288a1da22558;
    nix.url = github:nixos/nix/6ff9aa8df7ce8266147f74c65e2cc529a1e72ce0;
    nixpkgs-mesa.url = github:nixos/nixpkgs-channels/bdac777becdbb8780c35be4f552c9d4518fe0bdb;
    lambda-launcher.url = github:balsoft/lambda-launcher;
    NUR = {
      url = github:nix-community/NUR;
      flake = false;
    };
    base16-unclaimed-schemes = {
      url = github:chriskempson/base16-unclaimed-schemes;
      flake = false;
    };
    home-manager.url = github:rycee/home-manager/bqv-flakes;
    materia-theme = {
      url = github:nana-4/materia-theme;
      flake = false;
    };
    nixpkgs-old = {
      url = github:nixos/nixpkgs/nixos-19.09;
      flake = false;
    };
    weechat-scripts = {
      url = github:weechat/scripts;
      flake = false;
    };
    simple-nixos-mailserver = {
      type = "git";
      url = "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver";
      ref = "master";
      flake = false;
    };
    nixpkgs-wayland = {
      url = github:colemickens/nixpkgs-wayland;
      flake = false;
    };
    weechat-notify-send = {
      url = github:s3rvac/weechat-notify-send;
      flake = false;
    };
    yt-utilities = {
      type = "git";
      url = "ssh://git@github.com/serokell/yt-utilities";
      ref = "flake";
    };
    nixos-fhs-compat.url = github:balsoft/nixos-fhs-compat;
  };

  outputs = { nixpkgs, nix, self, ... }@inputs: {
    nixosConfigurations = with nixpkgs.lib;
      let
        hosts = map (fname: builtins.head (builtins.match "(.*)\\.nix" fname))
          (builtins.attrNames (builtins.readDir ./hardware-configuration));
        mkHost = name:
          nixosSystem {
            system = "x86_64-linux";
            modules = [ (import ./default.nix) ];
            specialArgs = { inherit inputs name; };
          };
      in genAttrs hosts mkHost;

    legacyPackages.x86_64-linux =
      (builtins.head (builtins.attrValues self.nixosConfigurations)).pkgs;

    # nix run github:serokell/deploy
    # Because sudo requires local presence of my Yubikey, we have to manually activate the system
    # sudo nix-env -p /nix/var/nix/profiles/system --set /nix/var/nix/profiles/per-user/balsoft/system;
    # sudo /nix/var/nix/profiles/system/bin/switch-to-configuration switch
    deploy = {
      user = "balsoft";
      nodes = builtins.mapAttrs (_: conf: {
        hostname = conf.config.networking.hostName;
        profiles.system.path = conf.config.system.build.toplevel;
      }) self.nixosConfigurations;
    };
  };
}
