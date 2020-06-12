;;; init.el --- balsoft's config

;;; Commentary:

;;; None

;; -*- lexical-binding: t -*-

;;; Code:

;;----------------------------------------------------------------------------
;; Adjust garbage collection
;;----------------------------------------------------------------------------
(setq gc-cons-threshold (* 20 1024 1024))


;; ---------------------
;; Setup Load Path
;; ---------------------

(nconc load-path
       (list (expand-file-name "local" user-emacs-directory)
	           (expand-file-name "wakib" user-emacs-directory)))


;; -----------------------
;; use-package
;; -----------------------
(setq load-prefer-newer t)              ; Don't load outdated byte code

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)

  (add-to-list 'package-archives '("cselpa" . "https://elpa.thecybershadow.net/packages/"))
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(setq package-enable-at-startup nil)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
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

(add-to-list 'load-path "~/.emacs.d/elisp/")

(require 'gud-lldb)

(use-package ws-butler
  :config
  (ws-butler-global-mode)
  )

(use-package auto-indent-mode
  :config
  (auto-indent-global-mode))

(global-auto-revert-mode)

(electric-indent-mode 1)

;; (use-package ergoemacs-mode
;;   :config
;;   (ergoemacs-mode 1))


(use-package xah-fly-keys
  :config
  (xah-fly-keys-set-layout "qwerty")
  (xah-fly-keys 1)
  (define-key xah-fly-insert-map (kbd "M-SPC") 'xah-fly-command-mode-activate)
  )

(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "C-TAB") 'org-cycle-level)

(global-display-line-numbers-mode)

(electric-pair-mode)

(electric-indent-mode)

(use-package flycheck
  :config
  (global-flycheck-mode))

;; -------------------
;; Initial Setup
;; -------------------
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(cua-selection-mode 1)
;;(define-key cua--rectangle-keymap (kbd "ESC") nil)
;;(define-key cua-global-keymap (kbd "<C-return>") nil)
;;(define-key cua-global-keymap (kbd "C-x SPC") 'cua-rectangle-mark-mode)




(when (not window-system)
  (xterm-mouse-mode 1))



(use-package hasklig-mode
  :hook (haskell-mode))



(use-package company-ghci
  :config
  (push 'company-ghci company-backends))

(global-set-key (kbd "M-RET") 'execute-extended-command)

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
(set 'pop-up-frames 'graphic-only)
(set 'gdb-use-separate-io-buffer nil)
(set 'gdb-many-windows nil)


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
  (setq projectile-completion-system 'ivy)
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

;; -------------------
;; Yasnippet
;; -------------------

(use-package nix-mode
  :hook
  ((nix-mode . (lambda () (local-set-key (kbd "<f7>") 'nix-format-buffer))))
  ((nix-mode . (lambda () (setq indent-line-function 'nix-indent-line)))))

;; -------------------
;; expand-region
;; -------------------
(use-package company
  :config
  (global-company-mode 1)
  ;; Trigger completion immediately.
  (setq company-idle-delay 0)

  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)

  ;; Use the tab-and-go frontend.
  ;; Allows TAB to select and complete at the same time.
  (company-tng-configure-default)
  (setq company-frontends
        '(company-tng-frontend
          company-echo-metadata-frontend))

  (require 'color)

  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face)))))))

(use-package company-tabnine
  :config
  (add-to-list 'company-backends #'company-tabnine))


;; -------------------
;; Ivy
;; -------------------
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (define-key ivy-minibuffer-map [remap keyboard-quit] 'minibuffer-keyboard-quit)
;;  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "")
  (setq ivy-initial-inputs-alist nil))

(use-package smex)

;; TODO (change defun rewrite to advice)
(use-package quickrun
  :init
  (global-set-key [menu-bar tools quickrun] `(menu-item ,"Run Buffer" quickrun))
  :config
  (setq quickrun-focus-p nil)
  ;; Move cursor out of the way when displaying output
  (advice-add 'quickrun--recenter
	      :after (lambda (&optional _)
		       (with-selected-window
			   (get-buffer-window quickrun--buffer-name)
			 (end-of-buffer))))
  :bind
  (([f8] . quickrun )))


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

(defun compile-on-save-start ()
  "Recompile when compilation is not going."
  (let ((buffer (compilation-find-buffer)))
    (unless (get-buffer-process buffer)
      (recompile))))

(define-minor-mode compile-on-save-mode
  "Minor mode to automatically call `recompile' whenever the
current buffer is saved. When there is ongoing compilation,
nothing happens."
  :lighter " CoS"
    (if compile-on-save-mode
    (progn  (make-local-variable 'after-save-hook)
        (add-hook 'after-save-hook 'compile-on-save-start nil t))
    (kill-local-variable 'after-save-hook)))

(auto-fill-mode)


;; -------------------
;; Theme
;; -------------------
(use-package xresources-theme
  :config
  (load-theme 'xresources t)
  (setq after-init-hook (load-theme 'xresources t))
  (add-hook 'after-make-frame-functions
            (lambda (frame)
                (with-selected-frame frame
                  (load-theme 'xresources t)
                  (set-frame-font "IBM Plex Mono 11" nil t)))))

(setq inhibit-startup-screen t)

;;; init.el ends here
