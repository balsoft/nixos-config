{ pkgs, config, lib, inputs, ... }:
with lib;
with types;
let
  secret = { name, ... }: {
    options = {
      encrypted = mkOption {
        type = path;
        default = "/home/balsoft/.password-store/${name}.gpg";
      };
      decrypted = mkOption {
        type = path;
        default = "/var/secrets/${name}";
      };
      decrypt = mkOption {
        default = pkgs.writeShellScript "gpg-decrypt" ''
          set -euo pipefail
          export GPG_TTY="$(tty)"
          ${pkgs.gnupg}/bin/gpg-connect-agent updatestartuptty /bye 1>&2
          ${pkgs.gnupg}/bin/gpg --batch --no-tty --decrypt
        '';
      };
      user = mkOption {
        type = str;
        default = "balsoft";
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
        default = [ "${name}" ];
      };
      __toString = mkOption {
        readOnly = true;
        default = s: s.decrypted;
      };
    };
  };

  decrypt = name: cfg:
    with cfg; {
      "${name}-secrets" = rec {

        requires = [ "user@1000.service" ];
        after = requires;

        preStart = ''
          stat '${encrypted}'
          mkdir -p '${builtins.dirOf decrypted}'
        '';

        script = ''
          if cat '${encrypted}' | /run/wrappers/bin/sudo -u ${user} ${cfg.decrypt} > '${decrypted}.tmp'; then
            mv -f '${decrypted}.tmp' '${decrypted}'
            chown '${owner}' '${decrypted}'
            chmod '${permissions}' '${decrypted}'
          else
            echo "Failed to decrypt the secret"
            rm '${decrypted}.tmp'
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
      requires = [ "${name}-secrets.service" ];
      after = requires;
      bindsTo = requires;
    });

  mkServices = name: cfg: [ (decrypt name cfg) (addDependencies name cfg) ];

  allServices = toString (map (name: "${name}-envsubst.service")
    (builtins.attrNames config.secrets-envsubst)
    ++ map (name: "${name}-secrets.service")
    (builtins.attrNames config.secrets));
in {
  options.secrets = lib.mkOption {
    type = attrsOf (submodule secret);
    default = { };
  };

  options.secretsConfig = {
    repo = lib.mkOption {
      type = str;
      default = "ssh://git@github.com/balsoft/pass";
    };
  };

  config.systemd.services =
    mkMerge (concatLists (mapAttrsToList mkServices config.secrets));

  config.environment.systemPackages = [
    (pkgs.writeShellScriptBin "activate-secrets" ''
      set -euo pipefail
      # Make sure card is available and unlocked
      echo fetch | gpg --card-edit --no-tty --command-fd=0
      ${pkgs.gnupg}/bin/gpg --card-status
      if [ -d "$HOME/.password-store" ]; then
        cd "$HOME/.password-store"; ${pkgs.git}/bin/git pull
      else
        ${pkgs.git}/bin/git clone ${lib.escapeShellArg config.secretsConfig.repo} "$HOME/.password-store"
      fi
      ln -sf ${
        pkgs.writeShellScript "push" "${pkgs.git}/bin/git push origin master"
      } "$HOME/.password-store/.git/hooks/post-commit"
      cat $HOME/.password-store/email/balsoft@balsoft.ru.gpg | ${pkgs.gnupg}/bin/gpg --decrypt > /dev/null
      sudo systemctl restart ${allServices}
    '')
  ];

  config.security.sudo.extraRules = [{
    users = [ "balsoft" ];
    commands = [{
      command = "/run/current-system/sw/bin/systemctl restart ${allServices}";
      options = [ "NOPASSWD" ];
    }];
  }];

  config.persist.derivative.directories =
    [ "/var/secrets" "/home/balsoft/.password-store" ];

  config.home-manager.users.balsoft = {
    wayland.windowManager.sway = {
      config.startup = [{ command = "activate-secrets"; }];
    };
  };
}
