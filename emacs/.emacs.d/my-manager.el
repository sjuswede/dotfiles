;; -*- lexical-binding: t -*-
;; my-manager.el for jesper@pobox.com
;;
;; This contains the massive definitions for my management helper
;; It relies on what has been setup in car-init.el
;; Right now that is;
;;
;; vertico
;; marginalia
;; orderless
;; consult
;; Possibly add embark?

(use-package vertico
  :custom
  ;; (setq vertico-scroll-margin 0) ;; Different scroll margin
  (setq vertico-count 20) ;; Show more candidates
  (setq vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (setq vertico-cycle t)  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  :bind
  ("<tab>" . vertico-insert)
  ("<escape>" . minibuffer-keyboard-quit)
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

;; Prompt indicator for `completing-read-multiple', because of lacking Emacs feature.
(when (< emacs-major-version 31)
  (advice-add #'completing-read-multiple :filter-args
              (lambda (args)
                (cons (format "[CRM%s] %s"
                              (string-replace "[ \t]*" "" crm-separator)
                              (car args))
                      (cdr args)))))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))



;; use package consult plx
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)


  (require 'consult)
  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)

  ;; Optionally configure a different project root function.
  ;; 1. project.el (the default)
  ;(setq consult-project-function #'consult--default-project--function)
  ;; 2. vc.el (vc-root-dir)
  ;(setq consult-project-function (lambda (_) (vc-root-dir)))
  ;; 3. locate-dominating-file
  ;(setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;; 4. projectile.el (projectile-project-root)
  ;(autoload 'projectile-project-root "projectile")
  ;(setq consult-project-function (lambda (_) (projectile-project-root)))
  ;; 5. Disable project support
  (setq consult-project-function nil)
)

;; Wrappers around consult-ripgrep and consult-find
;; If in dired/dirvish, they search the currently selected files/directories.
;; If not in dired, they search the whole current project.
;; This is great for filtering out candidates based on a subset of files in a project.
;;
(defun mag/consult-project-or-dired-ripgrep ()
"If in dired, call `consult-ripgrep' for the currently selected files.  Otherwise call `consult-ripgrep` on the current project."
(interactive)
(if (eq major-mode 'dired-mode)
    (let ((consult-project-function (lambda (x) nil)))
      (consult-ripgrep (mapconcat #'shell-quote-argument (dired-get-marked-files) " ")))
  (consult-ripgrep)
  ))
;;
(defun mag/consult-project-or-dired-find ()
"If in dired, call `consult-find' for the currently selected files.  Otherwise call `consult-find` on the current project."
(interactive)
(if (eq major-mode 'dired-mode)
    (let ((consult-project-function (lambda (x) nil)))
      (consult-find (mapconcat #'shell-quote-argument (dired-get-marked-files) " ")))
  (consult-find)
  ))

;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
	(apply (if vertico-mode
		   #'consult-completion-in-region
		 #'completion--in-region)
	       args)))

;; Add Org buffers list to consult-buffer-source, and allow creation of new
(defvar org-source
  (list :name     "Org Buffer"
        :category 'buffer
        :narrow   ?o
        :face     'consult-buffer
        :history  'buffer-name-history
        :state    #'consult--buffer-state
        :new
        (lambda (name)
          (with-current-buffer (get-buffer-create name)
            (insert "#+title: " name "\n\n")
            (org-mode)
            (consult--buffer-action (current-buffer))))
        :items
        (lambda ()
          (consult--buffer-query :mode 'org-mode :as #'consult--buffer-pair))))
;;
(add-to-list 'consult-buffer-sources 'org-source 'append)

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :after vertico
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators light nil))
  (marginalia-align 'right)
  (marginalia-max-relative-age 0)
  :init
  (marginalia-mode))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; EOF
