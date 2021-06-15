{ pkgs, config, lib, inputs, ... }:
with lib;
with types;
let
  envsubstSecrets = { name, ... }: {
    options = {
      directory = mkOption {
        type = nullOr str;
        default = name;
      };
      secrets = mkOption { type = listOf str; };
      template = mkOption { type = str; };
      prefix = mkOption {
        type = nullOr str;
        default = null;
      };
      substituted = mkOption {
        type = path;
        default = "/var/secrets/${name}-envsubst";
      };
      envsubst = mkOption {
        type = str;
        default = "${pkgs.envsubst}/bin/envsubst -no-unset -no-empty";
      };
      owner = mkOption {
        type = str;
        default = "root:root";
      };
      permissions = mkOption {
        type = lib.types.addCheck lib.types.str
          (perm: !isNull (builtins.match "[0-7]{3}" perm));
        default = "400";
      };
      services = mkOption {
        type = listOf str;
        default = [ "${name}.service" ];
      };
      __toString = mkOption {
        readOnly = true;
        default = s: s.substituted;
      };
    };
  };

  exportSecrets = name: cfg:
    let prefix = lib.optionalString (!isNull cfg.prefix) "${cfg.prefix}_";
    in map (secret:
      ''
        export ${prefix}${secret}="$(cat ${
          config.secrets."${name}-envsubst-${secret}".decrypted
        })"'') cfg.secrets;

  envsubst = name: cfg:
    with cfg; {
      "${name}-envsubst" = rec {

        requires = [ "user@1000.service" ]
          ++ map (secret: "${name}-envsubst-${secret}-secrets.service")
          cfg.secrets;
        after = requires;
        bindsTo = requires;

        preStart = "mkdir -p '${builtins.dirOf substituted}'";

        script = ''
          ${builtins.concatStringsSep "\n" (exportSecrets name cfg)}

          if cat '${
            builtins.toFile "template" template
          }' | ${cfg.envsubst} > '${substituted}.tmp'; then
            mv -f '${substituted}.tmp' '${substituted}'
            chown '${owner}' '${substituted}'
            chmod '${permissions}' '${substituted}'
          else
            echo "Failed to run the substition"
            rm '${substituted}.tmp'
            exit 1
          fi
        '';

        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = "yes";
        };
      };
    };

  addDependencies = name: cfg:
    with cfg;
    genAttrs services (service: rec {
      requires = [ "${name}-envsubst" ];
      after = requires;
      bindsTo = requires;
    });
  mkServices = name: cfg: [ (envsubst name cfg) (addDependencies name cfg) ];

  mkIndividualSecrets = name: cfg:
    map (x: {
      "${name}-envsubst-${x}" = {
        encrypted = "${config.environment.sessionVariables.PASSWORD_STORE_DIR}/${
            lib.optionalString (!isNull cfg.directory) "${cfg.directory}/"
          }${x}.gpg";
        services = [ ];
      };
    }) cfg.secrets;
in {
  options.secrets-envsubst = lib.mkOption {
    type = attrsOf (submodule envsubstSecrets);
    default = { };
  };
  config.systemd.services =
    mkMerge (concatLists (mapAttrsToList mkServices config.secrets-envsubst));
  config.secrets = mkMerge
    (concatLists (mapAttrsToList mkIndividualSecrets config.secrets-envsubst));
}
