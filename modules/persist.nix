{ config, pkgs, lib, inputs, ... }:
let
  cfg = config.persist;

  takeAll = what: concatMap (x: x.${what});

  persists = with cfg; [ state derivative cache ];

  absoluteHomeFiles = map (x: "${cfg.homeDir}/${x}");

  allHomeFiles = takeAll "homeFiles" persists;

  absoluteEtcFiles = map (x: "/etc/${x}");

  allEtcFiles = absoluteEtcFiles (takeAll "etcFiles" persists);

  allDirectories = takeAll "directories" persists;

  inherit (builtins) concatMap;
  inherit (lib) mkIf;

in {
  options = let
    inherit (lib) mkOption mkEnableOption;
    inherit (lib.types) listOf path str;
    common = {
      directories = mkOption {
        type = listOf path;
        default = [ ];
      };
      etcFiles = mkOption {
        type = listOf str;
        default = [ ];
      };
      homeFiles = mkOption {
        type = listOf str;
        default = [ ];
      };
    };
  in {
    persist = {

      enable = mkEnableOption "a tmpfs root with explicit opt-in state";

      persistRoot = mkOption {
        type = path;
        default = "/persist";
      };

      homeDir = mkOption {
        type = path;
        default = "/home/balsoft";
      };

      # Stuff that matters
      # TODO backups of this stuff
      state = {
        # backup = {...};
      } // common;

      # Stuff that can be computed from declarative+state, but is never invalidated (so shouldn't be cleaned up)
      derivative = common;

      # Stuff that's just there to speed up the system
      # It's cleaned up regularly, to solve the cache invalidation problem once and for all
      cache = {
        clean = {
          enable = mkEnableOption "cleaning the cache files and directories";
          dates = mkOption {
            type = str;
            default = "weekly";
            description =
              "A systemd.time calendar description of when to clean the cache files";
          };
        };
      } // common;

    };
  };

  imports = [
    inputs.impermanence.nixosModules.impermanence
    # Eugh
    (let
      module = (import "${inputs.impermanence}/home-manager.nix" {
        inherit pkgs lib;
        config = lib.recursiveUpdate config.home-manager.users.balsoft {
          home.persistence."${cfg.persistRoot}${cfg.homeDir}" = {
            enable = true;
            directories = [ ];
            files = allHomeFiles;
            allowOther = false;
            removePrefixDirectory = false;
          };
        };
      });
    in {
      config.home-manager.users.balsoft = lib.mkIf cfg.enable module.config;
    })
  ];

  config = mkIf cfg.enable {
    environment.persistence.${cfg.persistRoot} = {
      directories = allDirectories;
      files = allEtcFiles;
    };

    fileSystems."/" = {
      device = "none";
      options = [ "defaults" "size=4G" "mode=755" ];
      fsType = "tmpfs";
    };

    boot.initrd.postMountCommands = assert config.fileSystems
      ? ${cfg.persistRoot}
      && config.fileSystems.${cfg.persistRoot}.neededForBoot; ''
        mkdir -p /mnt-root/nix
        mount --bind /mnt-root${cfg.persistRoot}/nix /mnt-root/nix
        chmod 755 /mnt-root
      '';

    # Euuuugh
    systemd.services.persist-cache-cleanup = lib.mkIf cfg.cache.clean.enable {
      description = "Cleaning up cache files and directories";
      script = ''
        ${builtins.concatStringsSep "\n" (map (x: "rm ${lib.escapeShellArg x}")
          (absoluteEtcFiles cfg.cache.etcFiles
            ++ absoluteHomeFiles cfg.cache.homeFiles))}

        ${builtins.concatStringsSep "\n"
        (map (x: "rm -rf ${lib.escapeShellArg x}") cfg.cache.directories)}
      '';
      startAt = cfg.cache.clean.dates;
    };

    system.activationScripts = {
      homedir.text = builtins.concatStringsSep "\n" (map (dir: ''
        mkdir -p ${cfg.persistRoot}${dir}
        chown balsoft:users ${cfg.persistRoot}${dir}
      '') (builtins.filter (lib.hasPrefix "/home/balsoft") allDirectories));
    };
  };
}
