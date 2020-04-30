{
  description =
    "A collection of crap, hacks and copy-paste to make my localhosts boot";

  edition = 201909;

  inputs = {
    nixpkgs-mopidy = {
      type = "github";
      owner = "NickHU";
      repo = "nixpkgs";
      ref = "mopidy";
      flake = false;
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
    mtxclient = {
      type = "github";
      owner = "nheko-reborn";
      repo = "mtxclient";
      ref = "84c6778cc367bca79755e73e77b2cc69950375b2";
      flake = false;
    };
    nheko = {
      type = "github";
      owner = "nheko-reborn";
      repo = "nheko";
      ref = "1943d1c74d5253ec9d2b7ee38f69d791d28e84c1";
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
    mobile-broadband-provider-info = {
      type = "git";
      url = "https://gitlab.gnome.org/GNOME/mobile-broadband-provider-info";
      flake = false;
    };
  };

  outputs = { nixpkgs, nix, ... }@inputs: {
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
  };
}
