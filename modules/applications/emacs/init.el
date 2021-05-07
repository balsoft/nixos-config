;;; init.el --- balsoft's config

;;; Commentary:

;;; None

;; -*- lexical-binding: t -*-

;;; Code:

(require 'package)

(package-initialize)

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure nil)

(setq-default tab-width 2)

(mode-line-bell-mode)

(line-number-mode)
(column-number-mode)

;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosave/" t)))

(setq-default indent-tabs-mode nil)


(setq-default tab-always-indent 'complete)


(setq-default compilation-scroll-output 'first-error)


(use-package ws-butler
  :config
  (ws-butler-global-mode)
  )

(global-auto-revert-mode)


(use-package xah-fly-keys
  :config
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)
  (define-key xah-fly-insert-map (kbd "M-SPC") 'xah-fly-command-mode-activate)
  )

(global-set-key (kbd "C-b") 'switch-to-buffer)

(global-display-line-numbers-mode)

(electric-pair-mode)

(electric-indent-mode)

(use-package flycheck
  :config
  (global-flycheck-mode))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(cua-selection-mode 1)

(setq auto-revert-check-vc-info t)

(vc-mode-line t)

(when (not window-system)
  (xterm-mouse-mode 1))



(use-package hasklig-mode
  :hook (haskell-mode))

(use-package company-ghci
  :config
  (push 'company-ghci company-backends))


(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning of line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive "^") ; Use (interactive) in Emacs 22 or older
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key [home] 'smart-beginning-of-line)

(global-unset-key [menu-bar options cua-mode])

(fringe-mode '(0 . 0))

;; scroll one line at a time (less "jumpy" than defaults)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time

(use-package frames-only-mode)
;; Make new frames instead of new windows
(setq pop-up-frames 'graphic-only)
(setq gdb-use-separate-io-buffer nil)
(setq gdb-many-windows nil)


(use-package counsel
  :config
  (counsel-mode 1))

;; -------------------
;; Projectile
;; -------------------
;; No deferred loading as bind-keymap
;; doesn't handle wakib C-d keymaps
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") nil)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode 1)
  (global-unset-key [menu-bar tools Projectile])
  (projectile-register-project-type 'nix-shell '("shell.nix")
                                    :compile "nix-build shell.nix"
                                    :run "nix-shell")
  (setq projectile-project-search-path '("~/projects/"))

  (global-set-key (kbd "<f9>") 'projectile-compile-project)
  (global-set-key (kbd "<f5>") 'projectile-run-project))

(use-package nix-mode
  :hook
  ((nix-mode . (lambda () (local-set-key (kbd "<f7>") 'nix-format-buffer))))
  ((nix-mode . (lambda () (setq indent-line-function 'nix-indent-line)))))

(use-package company
  :config
  (global-company-mode 1)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)
  (setq company-idle-delay 0)
  )

(use-package company-box
  :hook (company-mode . company-box-mode))


;; -------------------
;; Ivy
;; -------------------
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (define-key ivy-minibuffer-map [remap keyboard-quit] 'minibuffer-keyboard-quit)
  (setq ivy-count-format "")
  (setq ivy-initial-inputs-alist nil))


(use-package smex)

(show-paren-mode 1)
;; TODO - MOVE Electric Pair Mode to user local


;; MAJOR MODES

(use-package markdown-mode
  :mode "\\.\\(m\\(ark\\)?down\\|md\\)$")


;; Setup Splash Screen
;; (setq inhibit-startup-screen t)
;; (setq-default major-mode 'fundamental-mode)
;; (setq-default initial-scratch-message "")

(setq custom-file (expand-file-name "custom" user-emacs-directory))
(load custom-file t t)

(auto-fill-mode)


;; -------------------
;; Theme
;; -------------------
;; (use-package xresources-theme
;;   :config
;;   (load-theme 'xresources t)
;;   (setq after-init-hook (load-theme 'xresources t))
;;   (add-hook 'after-make-frame-functions
;;             (lambda (frame)
;;                 (with-selected-frame frame
;;                   (load-theme 'xresources t)
;;                   (set-frame-font "IBM Plex Mono 11" nil t)))))

(add-to-list 'default-frame-alist '(font . "IBM Plex Mono 11"))
(set-face-attribute 'default nil :font "IBM Plex Mono 11")

(setq inhibit-startup-screen t)

(use-package lsp-mode
  :config
  (add-hook 'rust-mode-hook 'lsp)
  )

(use-package lsp-haskell
  :config
  (add-hook 'haskell-mode-hook 'lsp)
  (add-hook 'haskell-literate-mode-hook 'lsp)
  )

(use-package envrc
  :config
  (envrc-global-mode)
  (advice-add 'lsp :before (lambda (&optional n) (envrc--update))))

(setq treemacs-position 'right)

(use-package treemacs)

(use-package lsp-treemacs)

(use-package treemacs-projectile)

(load-theme 'manoj-dark t)
;;; init.el ends here
