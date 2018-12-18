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
(use-package diminish)

(mode-line-bell-mode)

;; -------------------
;; Wakib
;; -------------------
(use-package wakib-keys
  :diminish wakib-keys
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

(global-set-key (kbd "C-w") 'kill-buffer)

(global-display-line-numbers-mode)

;; -------------------
;; Initial Setup
;; -------------------
(menu-bar-mode -1) 
(scroll-bar-mode -1)

(cua-selection-mode 1)
;;(define-key cua--rectangle-keymap (kbd "ESC") nil)
;;(define-key cua-global-keymap (kbd "<C-return>") nil)
;;(define-key cua-global-keymap (kbd "C-x SPC") 'cua-rectangle-mark-mode)

(advice-add 'substitute-command-keys :around #'wakib-substitute-command-keys)

(use-package hasklig-mode
  :hook (haskell-mode))

(server-start)


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
(use-package nord-theme
  :config
  (add-hook 'after-make-frame-functions
        (lambda (frame)
            (with-selected-frame frame
              (load-theme 'nord t)
	      )))
  (load-theme 'nord t))
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
;; -------------------
;; Magit
;; -------------------
(use-package magit
  :bind
  (("C-x g" . magit-status )))



(use-package exec-path-from-shell
  :disabled
  :config
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))
    

;; -------------------
;; Ivy
;; -------------------
(use-package ivy
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (define-key ivy-minibuffer-map [remap keyboard-quit] 'minibuffer-keyboard-quit)
;;  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "")
  (setq ivy-initial-inputs-alist nil))

(use-package counsel
  :diminish counsel-mode
  :config
  (counsel-mode 1)
  (define-key wakib-keys-overriding-map (kbd "C-S-v") 'counsel-yank-pop))



;; find out what ivy uses from smex
(use-package smex)
;; -------------------
;; Projectile
;; -------------------
;; No deferred loading as bind-keymap
;; doesn't handle wakib C-d keymaps
(use-package projectile
  :diminish projectile-mode
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
  (global-unset-key [menu-bar tools Projectile]))

;; -------------------
;; Yasnippet
;; -------------------

(use-package yasnippet-snippets
  :defer t)

(use-package yasnippet
  :hook
  ((prog-mode . yas-minor-mode))
  :diminish yas-minor-mode
  :config
  (require 'yasnippet-snippets)
  (yas-reload-all)
  (define-key yas-keymap [remap wakib-next] 'yas-next-field)
  (define-key yas-keymap [remap wakib-previous] 'yas-prev-field))


(use-package ivy-yasnippet
  :bind ("C-y" . ivy-yasnippet))


;; -------------------
;; expand-region
;; -------------------
(use-package company               
  :diminish company-mode
  :config
  (global-company-mode 1)
  (define-key company-active-map [remap wakib-next] 'company-select-next)
  (define-key company-active-map [remap wakib-previous] 'company-select-previous))

;; -------------------
;; expand-region
;; -------------------
(use-package expand-region
  :bind ("M-A" . er/expand-region))

;; -------------------
;; avy
;; -------------------
(use-package avy
  :bind ("M-m" . avy-goto-char-2))

;; -------------------
;; switch-window
;; -------------------
(use-package switch-window
  :bind ("M-H" . switch-window)
  :config
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-threshold 1))

;; -------------------
;; which-key
;; -------------------
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

;; -------------------
;; multiple-cursors
;; -------------------
;; TODO - Advice CUA-keyboard-quit to quit mc and rrm
(use-package multiple-cursors
  :init
  (custom-set-variables `(mc/always-run-for-all ,t))
  :config
  (define-key mc/keymap [remap keyboard-quit] 'mc/keyboard-quit)
  (define-key rectangular-region-mode-map [remap keyboard-quit] 'rrm/keyboard-quit)
  ;;(custom-set-variables `(mc/always-run-for-all ,t))
  :bind
  (("M-S-s" . set-rectangular-region-anchor)
   :map wakib-keys-overriding-map
	("C-." . mc/mark-next-like-this)
	("C-," . mc/mark-previous-like-this)
	("<C-down-mouse-1>" . mc/add-cursor-on-click)))

;; -------------------
;; diff-hl
;; -------------------
(use-package diff-hl
  :hook
  ((prog-mode . turn-on-diff-hl-mode)
   (magit-post-refresh-hook . diff-hl-magit-post-refresh)))

(use-package powerline
  :config
  (powerline-default-theme)
  )

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


;; Better Parenthesis
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(show-paren-mode 1)
;; TODO - MOVE Electric Pair Mode to user local


;; MAJOR MODES

(use-package markdown-mode
  :mode "\\.\\(m\\(ark\\)?down\\|md\\)$")


;; Setup Splash Screen
(setq inhibit-startup-screen t)
(setq-default major-mode 'org-mode)
(setq-default initial-scratch-message ";; Emacs lisp scratch buffer. Happy hacking.\n\n")

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

(require 'init-local nil t)
