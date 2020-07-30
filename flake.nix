{
  description =
    "A collection of crap, hacks and copy-paste to make my localhosts boot";

  inputs = {
    lambda-launcher = {
      type = "github";
      owner = "balsoft";
      repo = "lambda-launcher";
    };
    NUR = {
      type = "github";
      owner = "nix-community";
      repo = "NUR";
      flake = false;
    };
    base16-unclaimed-schemes = {
      type = "github";
      owner = "chriskempson";
      repo = "base16-unclaimed-schemes";
      flake = false;
    };
    home-manager = {
      type = "github";
      owner = "rycee";
      repo = "home-manager";
      ref = "bqv-flakes";
    };
    materia-theme = {
      type = "github";
      owner = "nana-4";
      repo = "materia-theme";
      flake = false;
    };
    nixpkgs-old = {
      type = "github";
      owner = "nixos";
      repo = "nixpkgs";
      ref = "nixos-19.03";
      flake = false;
    };
    weechat-scripts = {
      type = "github";
      owner = "weechat";
      repo = "scripts";
      flake = false;
    };
    simple-nixos-mailserver = {
      type = "git";
      url = "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver";
      ref = "master";
      flake = false;
    };
    nixpkgs-wayland = {
      type = "github";
      owner = "colemickens";
      repo = "nixpkgs-wayland";
      flake = false;
    };
    weechat-notify-send = {
      type = "github";
      owner = "s3rvac";
      repo = "weechat-notify-send";
      flake = false;
    };
    yt-utilities = {
      type = "git";
      url = "ssh://git@github.com/serokell/yt-utilities";
      ref = "flake";
    };
    nixos-fhs-compat = {
      type = "github";
      owner = "balsoft";
      repo = "nixos-fhs-compat";
    };
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
