{pkgs, config, lib, ...}:
{
  home-manager.users.balsoft =
  {
    home.packages = with pkgs; [irony-server clang];
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
      ];
    };
    xdg.dataFile."applications/emacsclient.desktop".source =
    pkgs.makeDesktopItem
    {
      terminal = "False";
      type = "Application";
      name = "emacsclient";
      genericName = "Text editor";
      desktopName = "Emacs client";
      mimeType = "text/plain";
      exec = "emacsclient -c %u";
      categories = "Development;TextEditor;";
      icon = "emacs";
    };
    home.file.".emacs.d/init.el".source = ./init.el;
    home.file.".emacs.d/elisp/gud-lldb.el".source = ./gud-lldb.el;
    home.activation.emacs =
    {
      before = [];
      after = [];
      data = "$DRY_RUN_CMD mkdir -p ~/.emacs.d/autosave";
    };
  };
}
