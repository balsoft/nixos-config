{
  description =
    "A collection of crap, hacks and copy-paste to make my localhosts boot";

  nixConfig.substituters = [ "https://cache.nixos.org" ];

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix.url = "github:nixos/nix";
    lambda-launcher.url = "github:balsoft/lambda-launcher";
    deploy-rs.url = "github:serokell/deploy-rs";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    NUR = {
      url = "github:nix-community/NUR";
      flake = false;
    };
    base16-black-metal-scheme = {
      url = "github:metalelf0/base16-black-metal-scheme";
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
    simple-nixos-mailserver = {
      url = "gitlab:simple-nixos-mailserver/nixos-mailserver";
      flake = false;
    };
    nixpkgs-wayland = {
      url = "github:colemickens/nixpkgs-wayland";
      flake = false;
    };
    nixos-fhs-compat.url = "github:balsoft/nixos-fhs-compat";
    simple-osd-daemons.url = "github:balsoft/simple-osd-daemons";
    sonoff-lan = {
      url = "github:AlexxIT/SonoffLAN";
      flake = false;
    };
    crdt = {
      url = "git+https://code.librehq.com/qhong/crdt.el";
      flake = false;
    };
    impermanence.url = "github:nix-community/impermanence";

    rycee = {
      url = "gitlab:rycee/nur-expressions";
      flake = false;
    };

    nix-direnv = { url = "github:nix-community/nix-direnv"; };

    nheko = {
      url = "github:nheko-reborn/nheko";
      flake = false;
    };

    mtxclient = {
      url = "github:nheko-reborn/mtxclient";
      flake = false;
    };

    coeurl = {
      url = "git+https://nheko.im/nheko-reborn/coeurl";
      flake = false;
    };

    mopidy-ytmusic = {
      url = "github:OzymandiasTheGreat/mopidy-ytmusic";
      flake = false;
    };

    brother-hl-3170cdw-driver = {
      url = "github:balsoft/brother-hl-3170cdw-driver";
      flake = false;
    };

    codebraid = {
      url = "github:gpoore/codebraid";
      flake = false;
    };

    flake-registry = {
      url = "github:nixos/flake-registry";
      flake = false;
    };

    nix-vscode-marketplace.url = "github:AmeerTaweel/nix-vscode-marketplace";

    remapper.url = "github:balsoft/remapper";

    helix.url = "github:helix-editor/helix";

    tridactyl-native-messenger = {
      url = "github:tridactyl/native_messenger";
      flake = false;
    };

    nixos-hardware.url = "github:balsoft/nixos-hardware/add-librem-5";

    yt-dlp.url = "github:yt-dlp/yt-dlp";
    yt-dlp.flake = false;
  };

  outputs = { nixpkgs, self, nix, deploy-rs, ... }@inputs:
    let
      findModules = dir:
        builtins.concatLists (builtins.attrValues (builtins.mapAttrs
          (name: type:
            if type == "regular" then [{
              name = builtins.elemAt (builtins.match "(.*)\\.nix" name) 0;
              value = dir + "/${name}";
            }] else if (builtins.readDir (dir + "/${name}"))
            ? "default.nix" then [{
              inherit name;
              value = dir + "/${name}";
            }] else
              findModules (dir + "/${name}")) (builtins.readDir dir)));
      pkgsFor = system:
        import inputs.nixpkgs {
          overlays = [ self.overlay ];
          localSystem = { inherit system; };
          config = { android_sdk.accept_license = true; };
        };
    in {
      nixosModules = builtins.listToAttrs (findModules ./modules);

      nixosProfiles = builtins.listToAttrs (findModules ./profiles);

      nixosRoles = import ./roles;

      nixosConfigurations = with nixpkgs.lib;
        let
          hosts = builtins.attrNames (builtins.readDir ./machines);

          mkHost = name:
            let
              system = builtins.readFile (./machines + "/${name}/system");
              pkgs = pkgsFor system;
            in nixosSystem {
              inherit system;
              modules = __attrValues self.nixosModules ++ [
                inputs.home-manager.nixosModules.home-manager

                {
                  disabledModules =
                    [ "services/x11/desktop-managers/plasma5.nix" ];
                }

                (import (./machines + "/${name}"))
                { nixpkgs.pkgs = pkgs; }
                { device = name; }
              ];
              specialArgs = { inherit inputs; };
            };
        in genAttrs hosts mkHost;

      legacyPackages.x86_64-linux = pkgsFor "x86_64-linux";

      defaultApp = deploy-rs.defaultApp;

      overlay = import ./overlay.nix inputs;

      lib = import ./lib.nix nixpkgs.lib;

      devShell.x86_64-linux = with nixpkgs.legacyPackages.x86_64-linux;
        mkShell {
          buildInputs = [
            nix.packages.x86_64-linux.default
            deploy-rs.defaultPackage.x86_64-linux
            nixfmt
          ];
          shellHook = ''
            linkFile() {
              source="$(nix build --print-out-paths "$1.source" || nix eval --raw "$1.source")"
              target="$(nix eval --raw "$1.target")"
              ln -fs "$source" "$HOME/$target"
            }
            linkHomeManagerFile() {
              linkFile ".#nixosConfigurations.$(hostname).config.home-manager.users.$(whoami).$1"
            }
            linkConfigFile() {
              linkHomeManagerFile "xdg.configFile.\"$1\""
            }
            linkDataFile() {
              linkHomeManagerFile "xdg.dataFile.\"$1\""
            }
            linkHomeFile() {
              linkHomeManagerFile "home.file.\"$1\""
            }
          '';
        };

      deploy = {
        user = "root";
        nodes = (builtins.mapAttrs (name: machine:
          let activateable = name == "T420-Laptop" || name == "RasPi-Server";
          in {
            hostname = machine.config.networking.hostName;
            profiles.system = {
              user = if activateable then "root" else "balsoft";
              path = with deploy-rs.lib.${machine.pkgs.system}.activate;
                if activateable then
                  nixos machine
                else
                  noop machine.config.system.build.toplevel;
            };
          }) self.nixosConfigurations);
      };
    };
}
