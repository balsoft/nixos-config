{ pkgs, config, lib, inputs, ... }:
with lib;
with types;
let
  envsubstSecrets = { name, ... }: {
    options = {
      directory = mkOption { type = str; };
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

  individualSecrets = name: cfg:
  map (x: builtins.elemAt (builtins.match "(.*).gpg" x) 0) (builtins.attrNames (lib.filterAttrs (n: v: v == "regular" || v == "symlink")
  (builtins.readDir (inputs.secrets + "/${cfg.directory}"))));

  exportSecrets = name: cfg:
    let prefix = lib.optionalString (!isNull cfg.prefix) "${cfg.prefix}_";
    in map (secret:
      ''
        export ${prefix}${secret}="$(cat ${
          config.secrets."${name}-envsubst-${secret}".decrypted
        })"'') (individualSecrets name cfg);

  envsubst = name: cfg:
    with cfg; {
      "${name}-envsubst" = rec {

        wantedBy = [ "multi-user.target" ];

        requires = [ "user@1000.service" "gpg-setup.service" ]
          ++ map (secret: "${name}-envsubst-${secret}-secrets.service") (individualSecrets name cfg);
        after = requires;

        preStart = "mkdir -p '${builtins.dirOf substituted}'";

        script = ''
          ${builtins.concatStringsSep "\n" (exportSecrets name cfg)}

          if cat '${builtins.toFile "template" template}' | ${cfg.envsubst} > '${substituted}.tmp'; then
            mv -f '${substituted}.tmp' '${substituted}'
            chown '${owner}' '${substituted}'
            chmod '${permissions}' '${substituted}'
          else
            echo "Failed to run the substition"
            rm '${substituted}.tmp'
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
    });
  mkServices = name: cfg: [ (envsubst name cfg) (addDependencies name cfg) ];

  mkIndividualSecrets = name: cfg:
    map (x: {
      "${name}-envsubst-${x}" = {
        encrypted = inputs.secrets + "/${cfg.directory}/${x}.gpg";
        services = [ ];
      };
    }) (individualSecrets name cfg);
in {
  options.secrets-envsubst =
    lib.mkOption { type = attrsOf (submodule envsubstSecrets); };
  config.systemd.services =
    mkMerge (concatLists (mapAttrsToList mkServices config.secrets-envsubst));
  config.secrets = mkMerge
    (concatLists (mapAttrsToList mkIndividualSecrets config.secrets-envsubst));
}
