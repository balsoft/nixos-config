;; -*- lexical-binding: t -*-

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

;; backup in one place. flat, no tree structure
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosave/" t)))

(setq-default indent-tabs-mode nil)


(setq-default tab-always-indent 'complete)


(setq compilation-scroll-output 'first-error)

(add-to-list 'load-path "~/.emacs.d/elisp/")

(require 'gud-lldb)

(use-package auto-indent-mode
  :config
  (auto-indent-global-mode))

(global-auto-revert-mode)

(electric-indent-mode 1)
;; -------------------
;; Wakib
;; -------------------
(use-package wakib-keys
  :config
  (wakib-keys 1)
  (add-hook 'after-change-major-mode-hook 'wakib-update-major-mode-map)
  (add-hook 'menu-bar-update-hook 'wakib-update-minor-mode-maps)
  ;; Modifying other modules
  ;; When remap is used for wakib-next and wakib-previous it no longer works
  (define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "C-S-f") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "M-;") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "M-:") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "C-v") 'isearch-yank-kill))

(global-set-key (kbd "C-w") 'kill-word)
(global-set-key (kbd "C-W") 'kill-buffer)

(global-display-line-numbers-mode)



(use-package ix)

(use-package clipmon
  :config
  (clipmon-mode))

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

(advice-add 'substitute-command-keys :around #'wakib-substitute-command-keys)



(when (not window-system)
  (xterm-mouse-mode 1))



(use-package hasklig-mode
  :hook (haskell-mode))



(use-package company-ghci
  :config
  (push 'company-ghci company-backends))

(global-set-key (kbd "M-RET") 'execute-extended-command)

;; (server-start)


(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive "^") ; Use (interactive) in Emacs 22 or older
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key [home] 'smart-beginning-of-line)

;; Menu Bars
;; TODO - Change bind-key to define-key
(bind-key [menu-bar file new-file]
	        `(menu-item "New File..." wakib-new-empty-buffer :enable (menu-bar-non-minibuffer-window-p)
		      :help "Create a new blank buffer"
		      :key-sequence ,(kbd "C-n")))

(bind-key [menu-bar file open-file]
	  `(menu-item "Open File..." find-file :enable (menu-bar-non-minibuffer-window-p)
                :help "Read an existing or new file from disk"
		      :key-sequence ,(kbd "C-o")))

(bind-key [menu-bar file dired]
	  `(menu-item "Open Directory..." dired :enable (menu-bar-non-minibuffer-window-p)
		      :help "Browse a directory, to operate on its files"
		      :keys "C-e d"))

(bind-key [menu-bar file insert-file]
	  `(menu-item "Insert File..." insert-file :enable (menu-bar-non-minibuffer-window-p)
		      :help "Insert another file into current buffer"
		      :keys "C-e i"))

(global-unset-key [menu-bar options cua-mode])

(fringe-mode '(0 . 0))

;; -------------------
;; Theme
;; -------------------
(use-package xresources-theme
  :config
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (when window-system
                (with-selected-frame frame
                  (load-theme 'xresources t))))))
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
  (counsel-mode 1)
  (define-key wakib-keys-overriding-map (kbd "C-S-v") 'counsel-yank-pop))

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
  (wakib-update-menu-map (global-key-binding [menu-bar tools Projectile])
			 projectile-command-map "C-e p")
  (define-key wakib-keys-map [menu-bar project]
	    `(menu-item ,"Project" ,(global-key-binding [menu-bar tools Projectile])
			:visible (projectile-project-p)))
  (define-key wakib-keys-map [menu-bar project seperator1] `(menu-item ,"--" nil))
  (define-key wakib-keys-map [menu-bar project git] `(menu-item ,"Git ..." magit-status :keys "C-e g"))
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

(use-package org-gcal
  :config
  (require 'org-gcal-config))

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
          company-pseudo-tooltip-frontend
          company-echo-metadata-frontend))

  (require 'color)

  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

  (define-key company-active-map [remap wakib-next] 'company-select-next)
  (define-key company-active-map [remap wakib-previous] 'company-select-previous))

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
(setq inhibit-startup-screen t)
(setq-default major-mode 'fundamental-mode)
(setq-default initial-scratch-message "")

;; Initial buffer choice causes split window when opening file from command line or
;; DE. While running wakib empty buffer causes profiling init file to fail
;;
;; (setq initial-buffer-choice (lambda (&optional _)
;; 			      (let ((buf (generate-new-buffer "untitled")))
;; 				(set-buffer-major-mode buf)
;; 				buf)))
(wakib-new-empty-buffer)

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
;;; init.el ends her
