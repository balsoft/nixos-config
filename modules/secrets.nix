{ pkgs, config, lib, inputs, ... }:
with lib;
with types;
let
  secret = { name, ... }: {
    options = {
      encrypted = mkOption {
        type = path;
        default = inputs.secrets + "/${name}.gpg";
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
        default = [ "${name}.service" ];
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

        wantedBy = [ "multi-user.target" ];

        requires = [ "user@1000.service" "gpg-setup.service" ];
        after = requires;

        preStart = ''
          [[ -r '${encrypted}' ]]
          mkdir -p '${builtins.dirOf decrypted}'
        '';

        script = ''
          ${optionalString (pkgs.gnupg.version == "2.2.24")
          "/run/wrappers/bin/sudo -u ${user} ${pkgs.gnupg}/bin/gpg --card-status"
          # 2.2.24 is broken and needs this hack for yubi to work
          }

          if cat '${encrypted}' | /run/wrappers/bin/sudo -u ${user} ${cfg.decrypt} > '${decrypted}.tmp'; then
            mv -f '${decrypted}.tmp' '${decrypted}'
            chown '${owner}' '${decrypted}'
            chmod '${permissions}' '${decrypted}'
          else
            echo "Failed to decrypt the secret"
            rm '${decrypted}.tmp'
          fi
        '';

        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = "yes";
        };

        unitConfig = {
          ConditionPathExists = [
            "/run/user/${
              toString config.users.users.${user}.uid
            }/gnupg/S.gpg-agent"
          ];
        };

      };
    };

  addDependencies = name: cfg:
    with cfg;
    genAttrs services (service: rec {
      requires = [ "${name}-secrets" ];
      after = requires;
    });

  gpg-setup = rec {

    wantedBy = [ "multi-user.target" ];

    requires = [ "user@1000.service" ];
    after = requires;

    path = [ pkgs.gnupg ];

    script = "echo fetch | gpg --card-edit --no-tty --command-fd=0";

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = "yes";
      User = "balsoft";
    };
  };

  mkServices = name: cfg: [ (decrypt name cfg) (addDependencies name cfg) ];
in {
  options.secrets = lib.mkOption { type = attrsOf (submodule secret); };
  config.systemd.services = mkMerge ([{ inherit gpg-setup; }]
    ++ concatLists (mapAttrsToList mkServices config.secrets));

  config.home-manager.users.balsoft = {
    systemd.user.services.pass-sync = {
      Install.WantedBy =
        [ "sway-session.target" ]; # Start when the gpg agent is ready

      Service = {
        ExecStartPre = toString (pkgs.writeShellScript "clone-pass-repo" ''
          set -euo pipefail

        '');
        ExecStart = toString (pkgs.writeShellScript "pass-sync" ''
          set -euo pipefail
          cd $HOME/.password-store
          while ${pkgs.inotifyTools}/bin/inotifywait "$HOME/.password-store/.git" -r -e modify -e close_write -e move -e create -e delete; do
            sleep 0.5
            ${pkgs.git}/bin/git push
          done
        '');
      };
    };
    wayland.windowManager.sway = {
      config.startup = [{
        command = toString (pkgs.writeShellScript "activate-secrets" ''
          set -euo pipefail
          # Make sure card is available and unlocked
          ${pkgs.gnupg}/bin/gpg --card-status
          cat ${inputs.secrets}/email/balsoft@balsoft.ru.gpg | ${pkgs.gnupg}/bin/gpg --decrypt
          systemctl restart '*-secrets.service' '*-envsubst.service'
          if [ -d "$HOME/.password-store" ]; then
            cd "$HOME/.password-store"; ${pkgs.git}/bin/git pull
          else
            ${pkgs.git}/bin/git clone ssh://git@github.com/balsoft/pass "$HOME/.password-store"
          fi
          ln -sf ${pkgs.writeShellScript "push" "${pkgs.git}/bin/git push origin master"} "$HOME/.password-store/.git/hooks/post-commit"
        '');
      }];
    };
  };
}
