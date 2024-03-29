{ config, pkgs, inputs, ... }:
let
  EDITOR = pkgs.writeShellScript "codium-editor" ''
    source "/etc/profiles/per-user/balsoft/etc/profile.d/hm-session-vars.sh"
    NIX_OZONE_WL=1 \
    exec \
    ${config.home-manager.users.balsoft.programs.vscode.package}/bin/codium \
    --enable-features=UseOzonePlatform \
    --ozone-platform=wayland \
    -w -n \
    "$@"
  '';
  codium-wayland = pkgs.buildEnv {
    name = "codium-wayland";
    paths = [
      (pkgs.writeShellScriptBin "codium-wayland" ''
        NIX_OZONE_WL=1 \
        exec \
        ${config.home-manager.users.balsoft.programs.vscode.package}/bin/codium \
        --enable-features=UseOzonePlatform \
        --ozone-platform=wayland \
        "$@"
      '')
      (pkgs.makeDesktopItem {
        name = "codium-wayland";
        desktopName = "VSCodium (Wayland)";
        exec = "codium-wayland";
        icon = "code";
        categories = [ "Utility" "TextEditor" "Development" "IDE" ];
        mimeTypes = [ "text/plain" "inode/directory" ];
        extraConfig = {
          StartupNotify = "true";
          StartupWMClass = "vscodium";
        };
      })
    ];
  };

  custom-extensions = import ./extensions.nix {
    inherit (pkgs.vscode-utils) buildVscodeMarketplaceExtension;
  };
in {
  environment.systemPackages = [ codium-wayland ];

  defaultApplications.editor = {
    cmd = "${EDITOR}";
    desktop = "codium-wayland";
  };
  home-manager.users.balsoft = {
    programs.vscode = {
      enable = true;
      package = pkgs.vscodium;

      mutableExtensionsDir = true;
      extensions =
        with inputs.nix-vscode-marketplace.packages.${pkgs.system}.vscode;
        with inputs.nix-vscode-marketplace.packages.${pkgs.system}.open-vsx;
        with pkgs.vscode-extensions;
        [
          kahole.magit

          cab404.vscode-direnv

          (pkgs.callPackage ./theme.nix { } config.themes.colors)

          vscodevim.vim

          matklad.rust-analyzer
          redhat.vscode-yaml
          jnoortheen.nix-ide
          dhall.dhall-lang
          hashicorp.terraform
          timonwong.shellcheck
          bungcip.better-toml
          haskell.haskell
          justusadam.language-haskell
          ms-python.python
          github.vscode-pull-request-github
          eamodio.gitlens
          llvm-vs-code-extensions.vscode-clangd
          stkb.rewrap
          shardulm94.trailing-spaces
          meraymond.idris-vscode
          ocamllabs.ocaml-platform
          bierner.markdown-mermaid
        ] ++ pkgs.lib.concatMap builtins.attrValues
        (builtins.attrValues custom-extensions);

      userSettings = {
        "update.mode" = "none";
        "[nix]"."editor.tabSize" = 2;
        "workbench.colorTheme" = "Balsoft's generated theme";
        "terminal.integrated.profiles.linux".bash.path =
          "/run/current-system/sw/bin/bash";
        "terminal.integrated.defaultProfile.linux" = "bash";
        "editor.fontFamily" = "IBM Plex Mono";
        "nix.formatterPath" = "nixfmt";
        "git.autofetch" = true;
        "redhat.telemetry.enabled" = false;
        "security.workspace.trust.untrustedFiles" = "open";
        "window.menuBarVisibility" = "toggle";
        "vim.useSystemClipboard" = true;
        "haskell.manageHLS" = "PATH";
        "extensions.autoCheckUpdates" = false;
        "extensions.autoUpdate" = false;
      };
      keybindings = [{
        key = "ctrl+shift+r";
        command = "workbench.action.tasks.runTask";
        when = "textInputFocus";
      }];
    };
  };
}
