{ pkgs, config, lib, ... }: {
  home-manager.users.balsoft = {
    programs.emacs = {
      enable = true;
      package = pkgs.emacs;
      extraPackages = epkgs:
        with epkgs; [
          use-package
          nix-mode
          haskell-mode
          wakib-keys
          exec-path-from-shell
          counsel
          projectile
          which-key
          markdown-mode
          frames-only-mode
          company
          rainbow-delimiters
          diff-hl
          mode-line-bell
          hasklig-mode
          flycheck
          flycheck-pkg-config
          auto-indent-mode
          clipmon
          org-gcal
          company-ghci
          xresources-theme
          company-tabnine
          expand-region
          ivy
          smex
          quickrun
          counsel-tramp
          ix
          magit
          xah-fly-keys
          arduino-mode
          elixir-mode
          company-box
          ws-butler
          yaml-mode
          gitlab-ci-mode
          gitlab-ci-mode-flycheck
          gitlab
          github-issues
          (github-pullrequest.overrideAttrs (oa: { buildInputs = oa.buildInputs ++ [pkgs.git]; }))
          scad-mode
          scad-preview
        ];
    };

    home.packages = [
      (pkgs.makeDesktopItem {
        terminal = "false";
        type = "Application";
        name = "emacsclient";
        genericName = "Text editor";
        desktopName = "Emacs client";
        mimeType = "text/plain";
        exec = "emacsclient -s /tmp/emacs1000/server -c %F";
        categories = "Development;TextEditor;Utility;";
        icon = "emacs";
      })
      pkgs.clang
    ];

    services.emacs.enable = true;

    systemd.user.services.emacs.Service.Environment =
      "PATH=/run/current-system/sw/bin:/etc/profiles/per-user/balsoft/bin";

    home.file.".emacs.d/init.el".source = ./init.el;
    home.activation.emacs = {
      before = [ ];
      after = [ ];
      data = "$DRY_RUN_CMD mkdir -p ~/.emacs.d/autosave";
    };
  };
}
