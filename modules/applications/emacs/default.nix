{pkgs, config, lib, ...}:
{
  home-manager.users.balsoft =
    {
      programs.emacs =
        {
          enable = true;
          package = pkgs.emacs;
          extraPackages = epkgs: with epkgs;
            [
              use-package
              nix-mode
              haskell-mode
              nixos-options
              nord-theme
              wakib-keys
              magit
              exec-path-from-shell
              ivy
              counsel
              smex
              projectile
              which-key
              markdown-mode
              diminish
              frames-only-mode
              company
              rainbow-delimiters
              diff-hl
              yasnippet
              yasnippet-snippets
              mode-line-bell
              powerline
              smart-mode-line-powerline-theme
              hasklig-mode
              irony
              flycheck
              flycheck-pkg-config
              auto-indent-mode
              ix
              clipmon
              org-gcal
              rust-mode
              #company-ghci
              company-lsp
              lsp-mode
              lsp-haskell
            ];
        };
      home.packages =
        [
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
          pkgs.irony-server
          pkgs.clang
        ];
      xsession.windowManager.i3.config.startup = [ {command = "emacs --daemon";} ];

      home.file.".emacs.d/init.el".source = ./init.el;
      home.file.".emacs.d/elisp/gud-lldb.el".source = ./gud-lldb.el;
      home.file.".emacs.d/elisp/org-gcal-config.el".text =
        ''
          ;;; org-gcal-config --- My org-gcal configuration
          ;;; Commentary:
          ;;; This is my configuration for org-gcal.
          ;;; Code:
          (setq org-gcal-client-id "${config.secrets.gcal.client-id}"
          org-gcal-client-secret "${config.secrets.gcal.client-secret}"
          org-gcal-file-alist '(("${config.secrets.gcal.email}" .  "~/Documents/agenda.org")))

          (provide 'org-gcal-config)
          ;;; org-gcal-config.el ends here
        '';
      home.activation.emacs =
        {
          before = [];
          after = [];
          data = "$DRY_RUN_CMD mkdir -p ~/.emacs.d/autosave";
        };
    };
}
