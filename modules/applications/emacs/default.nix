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
          ix
          clipmon
          org-gcal
          company-ghci
          xresources-theme
          company-tabnine
          expand-region
          ivy
          smex
          quickrun
          ranger
          dired-ranger
          ergoemacs-mode
        ];
    };

    home.packages = [
      (pkgs.makeDesktopItem {
        terminal = "False";
        type = "Application";
        name = "emacsclient";
        genericName = "Text editor";
        desktopName = "Emacs client";
        mimeType = "text/plain";
        exec = "emacsclient -c %F";
        categories = "Development;TextEditor;";
        icon = "emacs";
      })
      pkgs.clang
    ];

    services.emacs.enable = true;

    systemd.user.services.emacs.Service.Environment = "PATH=/run/current-system/sw/bin:/etc/profiles/per-user/balsoft/bin";

    home.file.".emacs.d/init.el".source = ./init.el;
    home.file.".emacs.d/elisp/gud-lldb.el".source = ./gud-lldb.el;
    home.activation.emacs = {
      before = [ ];
      after = [ ];
      data = "$DRY_RUN_CMD mkdir -p ~/.emacs.d/autosave";
    };
  };
}
