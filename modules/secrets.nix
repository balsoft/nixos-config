{ pkgs, config, lib, inputs, ... }:
with lib;
with types;
let
  password-store = config.secretsConfig.password-store;
  secret = { name, ... }: {
    options = {
      encrypted = mkOption {
        type = path;
        default = "${password-store}/${name}.gpg";
      };
      decrypted = mkOption {
        type = path;
        default = "${config.secretsConfig.decryptedDir}/${name}";
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

  restartAll = "${pkgs.systemd}/bin/systemctl restart ${allServices}";

  activate-secrets = pkgs.writeShellApplication {
    name = "activate-secrets";
    text = ''
      set -euo pipefail
      # wait for network
      while ! ping -c1 github.com; do sleep 1; done
      # Make sure card is available and unlocked
      echo fetch | gpg --card-edit --no-tty --command-fd=0
      gpg --card-status
      SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
      export SSH_AUTH_SOCK
      if [ -d "${password-store}/.git" ]; then
        cd "${password-store}"; git pull
      else
        git clone ${
          lib.escapeShellArg config.secretsConfig.repo
        } "${password-store}"
      fi
      gpg --decrypt < ${password-store}/unlock.gpg > /dev/null
      /run/wrappers/bin/sudo ${restartAll}
    '';
    runtimeInputs = [ pkgs.gnupg pkgs.git pkgs.systemd pkgs.openssh pkgs.iputils pkgs.coreutils ];

  };
  decrypt = name: cfg:
    with cfg; {
      "${name}-secrets" = rec {

        wantedBy = [ "multi-user.target" ];

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
            if [[ -f '${decrypted}' ]]; then
              echo "The decrypted file exists anyways, not failing"
              exit 0
            else
              exit 1
            fi
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
  options = {
    secrets = lib.mkOption {
      type = attrsOf (submodule secret);
      default = { };
    };

    secretsConfig = {
      password-store = lib.mkOption {
        type = lib.types.path;
        default = "/home/balsoft/.local/share/password-store";
      };
      repo = lib.mkOption {
        type = str;
        default = "ssh://git@github.com/balsoft/pass";
      };
      decryptedDir = lib.mkOption {
        type = lib.types.path;
        default = "/var/secrets";
      };
    };
  };

  config = {

    systemd.services =
      mkMerge (concatLists (mapAttrsToList mkServices config.secrets));

    security.sudo.extraRules = [{
      users = [ "balsoft" ];
      commands = [{
        command = restartAll;
        options = [ "NOPASSWD" ];
      }];
    }];

    persist.derivative.directories = [ config.secretsConfig.decryptedDir password-store ];

    home-manager.users.balsoft = {
      systemd.user.services.activate-secrets = {
        Service = {
          ExecStart = "${activate-secrets}/bin/activate-secrets";
          Type = "oneshot";
        };
        # Unit = { PartOf = [ "graphical-session-pre.target" ]; };
        # Install.WantedBy = [ "graphical-session-pre.target" ];
      };
      systemd.user.services.pass-store-sync = {
        Service = {
          Environment = [
            "PASSWORD_STORE_DIR=${password-store}"
            "PATH=${
              lib.makeBinPath [ pkgs.pass pkgs.inotify-tools pkgs.gnupg ]
            }"
          ];
          ExecStart = toString (pkgs.writeShellScript "pass-store-sync" ''
            export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
            while inotifywait "$PASSWORD_STORE_DIR" -r -e move -e close_write -e create -e delete --exclude .git; do
              sleep 0.1
              {
                pass git add --all
                pass git commit -m "Change"
                pass git pull --rebase
                pass git push
              } &
             done
          '');
        };
        Unit = rec {
          After = [ "activate-secrets.service" ];
          Wants = After;
        };
        Install.WantedBy = [ "graphical-session-pre.target" ];
      };
      programs.password-store = {
        enable = true;
        package = pkgs.pass-wayland;
        settings.PASSWORD_STORE_DIR = password-store;
      };
    };
  };
}
