{ config, pkgs, inputs, ... }: {
  defaultApplications.editor = {
    cmd =
      "${config.home-manager.users.balsoft.programs.vscode.package}/bin/codium";
    desktop = "codium";
  };
  home-manager.users.balsoft = {
    programs.vscode = {
      enable = true;
      package = pkgs.vscodium;

      mutableExtensionsDir = false;
      extensions = with pkgs.vscode-extensions; [
        vscodevim.vim
        kahole.magit
        (inputs.direnv-vscode.packages.${pkgs.system}.vsix.overrideAttrs (_: {
          buildPhase = "yarn run build";
          installPhase =
            "mkdir -p $out/share/vscode/extensions/direnv.direnv-vscode; cp -R * $out/share/vscode/extensions/direnv.direnv-vscode";
        }))

        (pkgs.callPackage ./theme.nix { } config.themes.colors)

        matklad.rust-analyzer
        redhat.vscode-yaml
        bbenoist.nix
        brettm12345.nixfmt-vscode
        dhall.dhall-lang
        hashicorp.terraform
        timonwong.shellcheck
      ];

      userSettings = {
        "update.channel" = "none";
        "[nix]"."editor.tabSize" = 2;
        "workbench.colorTheme" = "Balsoft's generated theme";
      };
    };
  };
}
