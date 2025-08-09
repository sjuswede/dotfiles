;; -*- lexical-binding: t -*-
;; org centric dotemacs for jesper@pobox.com

;; This file should included from init.el

;; Both packages and code snippets are here for now
;; A next step would be to put the code in another file
;; Maybe even in several files depending on purpose
;; That is a project for a rainy day

;; Performance improvement
(setenv "LSP_USE_PLISTS" "true")
(let ((file-name-handler-alist nil)
      (gc-cons-percentage .6)
      (gc-cons-threshold most-positive-fixnum)
      (mode-line-format nil)
      (read-process-output-max (* 1024 1024))))
(setq lsp-log-io nil) ; if set to true can cause a performance hit
;;
;; SETUP PACKAGE SYSTEM
;;

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     )

;(setq package-enable-at-startup nil)
(package-initialize)

;; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

;; Unconditionally refresh packages
;(package-refresh-contents)

;; Special treatment for use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t
	use-package-always-defer t))

(custom-set-variables '(load-prefer-newer t))
(use-package auto-compile
  :defer nil
  :ensure t
  :config (auto-compile-on-load-mode))

;(use-package use-package-ensure-system-package
 ; :ensure t)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;;
;; MY OWN FUNCTIONS AND SETTINGS
;;

;; Put backup and autosave files in one place
(let ((backup-dir "~/.local/emacs/backups")
      (auto-saves-dir "~/.local/emacs/auto-saves/"))
  (dolist (dir (list backup-dir auto-saves-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-saves-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,backup-dir))
        tramp-auto-save-directory auto-saves-dir))

;; Sane backup management
(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

;; This tells Emacs to put all backups in ~/.emacs.d/backups.
(custom-set-variables
 '(backup-directory-alist
   `(("." . ,(concat user-emacs-directory "backups")))))

;; If visiting a file whos directory does not exist, offer to create directory
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

;; Bind the above function to make it happen automagically
(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)


;; Basic settings
(setq global-visual-line-mode 1)
(custom-set-variables '(confirm-kill-processes nil))
(custom-set-variables '(mouse-yank-at-point t))
(custom-set-variables '(indent-tabs-mode nil))
(blink-cursor-mode 0)
(setq column-number-mode t)             ; Show column
(cua-mode t)                            ; I love CUA
(setq cua-auto-tabify-rectangles nil)   ; Don't tabify after rectangle commands
(transient-mark-mode 1)                 ; No region when it is not highlighted
(setq cua-keep-region-after-copy t)     ; Standard Windows behaviour
(setq auto-revert-interval 1            ; Refresh buffers fast
      echo-keystrokes 0.1               ; Show keystrokes asap
      inhibit-startup-message t         ; No splash screen please
      initial-scratch-message nil       ; Clean scratch buffer
      recentf-max-saved-items 100       ; Show more recent files
      ring-bell-function 'ignore        ; Quiet
      sentence-end-double-space nil)    ; No double space
(setq vc-follow-symlinks t)

;; Remove scrollbars
(when (display-graphic-p)
      (scroll-bar-mode 0))
(tool-bar-mode 0)
(menu-bar-mode 0)

;;
;; PACKAGES
;;

;; Icons, icons, icons
;; This is required for icons in other packages
;; Maybe I should use after/before and such here? TODO
(use-package all-the-icons
  :if (display-graphic-p))

;; Nice typewriter style mode for long writing
(use-package olivetti)
(global-set-key "\C-co" 'olivetti-mode)

;; Better Emacs help
(use-package helpful
  :config
  (setq elisp-refs-verbose nil)
  :bind
  ([remap describe-key]      . helpful-key)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-callable))

;; ripgrep for fast searches
(use-package rg
  :config (rg-enable-default-bindings)
  :bind
  ("C-c s" . rg-menu))

;; better fill and unfill
(use-package unfill
  :bind
  ("M-q" . unfill-toggle)
  ("A-q" . unfill-paragraph))

;; Which shows possible key commands
(use-package which-key
  :defer nil
  :diminish which-key-mode
  :config (which-key-mode))

;; Include the org mode stuff

;; my-org.el

(load "~/.emacs.d/my-org")

;; include the Helm/Ivy/Vertico file with all the nummies

;; my-manager.el

(load "~/.emacs.d/my-manager")

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

;;
;; THEME MANAGEMENT
;;

(defun jas/disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defvar jas/theme-hooks nil
  "((theme-id . function) ...)")

(defun jas/add-theme-hook (theme-id hook-func)
  (add-to-list 'jas/theme-hooks (cons theme-id hook-func)))

(defun jas/load-theme-advice (f theme-id &optional no-confirm no-enable &rest args)
  "Enhances `load-theme' in two ways:
1. Disables enabled themes for a clean slate.
2. Calls functions registered using `jas/add-theme-hook'."
  (unless no-enable
    (jas/disable-all-themes))
  (prog1
      (apply f theme-id no-confirm no-enable args)
    (unless no-enable
      (pcase (assq theme-id jas/theme-hooks)
        (`(,_ . ,f) (funcall f))))))

(advice-add 'load-theme
            :around
            #'jas/load-theme-advice)

(use-package zenburn-theme
  :ensure t
  :defer t)

(use-package material-theme
  :ensure t
  :defer t
  :init
  (defun jas/material-theme-hook ()
    (set-face-attribute 'which-key-key-face nil :foreground
                        (face-attribute 'error :foreground))
    (with-eval-after-load 'org-faces
      (cl-loop for n from 1 to 8
               do (set-face-attribute (intern-soft (format "org-level-%s" n))
                                      nil
                                      :height     'unspecified
                                      :background 'unspecified
                                      :box        'unspecified))))
  (jas/add-theme-hook 'material       #'jas/material-theme-hook)
  (jas/add-theme-hook 'material-light #'jas/material-theme-hook))

(use-package solarized
  :ensure solarized-theme
  :defer t
  :init
  (defun jas/solarized-theme-hook ()
    (set-face-attribute 'font-lock-constant-face nil :weight 'normal)
    (set-face-attribute 'font-lock-function-name-face nil :weight 'bold)
    (set-face-attribute 'which-key-key-face nil :foreground
                        (face-attribute 'error :foreground)))
  (jas/add-theme-hook 'solarized-dark  #'jas/solarized-theme-hook)
  (jas/add-theme-hook 'solarized-light #'jas/solarized-theme-hook)
  :config
  (setq solarized-use-variable-pitch nil
        solarized-use-less-bold t
        solarized-use-more-italic nil
        solarized-distinct-doc-face t
        solarized-high-contrast-mode-line t
        ;; I find different font sizes irritating.
        solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0))

(use-package hydra
  :ensure t
  :config
  (setq hydra-lv nil))

  (defhydra jas/themes-hydra (:hint nil :color pink)
  "
Themes

^Solarized^   ^Material^   ^Other^
----------------------------------------------------
_s_: Dark     _m_: Dark    _z_: Zenburn  _DEL_: none
_S_: Light    _M_: Light
"
  ("s" (load-theme 'solarized-dark  t))
  ("S" (load-theme 'solarized-light t))
  ("m" (load-theme 'material        t))
  ("M" (load-theme 'material-light  t))
  ("z" (load-theme 'zenburn         t))
  ("DEL" (jas/disable-all-themes))
  ("RET" nil "done" :color blue))

(bind-keys ("C-c w t"  . jas/themes-hydra/body))

;; Now kick up theme, if in graphical mode
(when (display-graphic-p)
  (load-theme 'material t))

;; Transparency management

(set-frame-parameter nil 'alpha-background 82) ; For current frame
(add-to-list 'default-frame-alist '(alpha-background . 82)) ; For all new frames henceforth

;; Toggle frame transparency for current frame only
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (frame-parameter nil 'alpha-background)
       100)
      (set-frame-parameter nil 'alpha-background '100)
    (set-frame-parameter nil 'alpha-background '82)))
(global-set-key (kbd "C-c t") 'toggle-transparency)

;;
;; CLEANUP
;;

(defconst 1mb 1048576)
(defconst 20mb 20971520)
(defconst 30mb 31457280)
(defconst 50mb 52428800)

;(defun fk/defer-garbage-collection ()
;  (setq gc-cons-threshold most-positive-fixnum))

(setq read-process-output-max 1mb)  ;; lsp-mode's performance suggest

(garbage-collect)

;; Math font test
;; ℕ𝓟⧺×≠≥≤±¬∨∧∃∀λ⟿⟹⊥⊤⊢
;; Japanese
;; およびイギリスのお客様に継続的なサービス利用環境を
;; Chinese
;; 中国的童鞋们的小伙伴都能发现你们哦
;; Emoji
;; 😭😤🤟🤘👨‍👨‍👧🧢⛑🪖💍💼🧤🧥🧦
;; Four byte UTF-8
;; 👻

;; EOF
