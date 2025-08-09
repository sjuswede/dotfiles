;; -*- lexical-binding: t -*-
;; Setup my org stuff - jesper@pobox.com
;;
;; Relies on basic settings in car-init.el

(add-to-list 'auto-mode-alist '("\\.\\(org\\  |org_archive\\|txt\\)$" . org-mode))
(add-to-list 'auto-mode-alist '("README$" . org-mode))

(add-hook 'org-mode-hook #'(lambda ()
                             (visual-line-mode)
                             (org-indent-mode)))

(use-package org-bullets
  :init (add-hook 'org-mode-hook (lambda ()
                                   (org-bullets-mode 1))))

;; My Agenda files
(setq org-agenda-files '("~/Dropbox/org/agenda"))

;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; always, t, or nil
(setq org-support-shift-select 't)

;; Bullet for org mode
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; EOF

