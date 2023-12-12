;;; init.el -*- lexical-binding: t; -*-

;(setq display-line-numbers-type t)

(use-package borg
  :load-path
  (lambda () (expand-file-name "lib/borg" user-emacs-directory))
  :config
  (when (native-comp-available-p)
    (setq borg-compile-function #'borg-byte+native-compile))
  (borg-initialize))

(use-package meow
 :config
 (load (expand-file-name "meow" user-emacs-directory)))

(use-package gruvbox-theme
 :config
 (load-theme 'gruvbox t))

(use-package dashboard
 :config
 (dashboard-setup-startup-hook))

(use-package doom-modeline
  :config
  (doom-modeline-mode 1))

(use-package nyan-mode
  :config
  (nyan-mode 1))

(use-package shackle
  :config
  (setq shackle-rules
	'(("^\\*\\([Hh]elp\\|Apropos\\)"
	   :regexp t :select t)))
  (shackle-mode))

(use-package helpful
  :bind
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command)
  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  ("C-c C-d" . helpful-at-point)
  ;; Look up *F*unctions (excludes macros).
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  ("C-h F" .  helpful-function))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package icomplete
  :no-require
  :config
  (icomplete-vertical-mode))

;; (setq org-directory "~/cloud/mobile/org")
;; (setq org-roam-directory org-directory)
;; (setq org-roam-dailies-directory "logbook")
;; (use-package org-roam
;;   :custom
;;   (org-roam-directory "~/cloud/mobile/org")
;;   ;(org-roam-dailies-directory "journals/")
;;   )
;(setq org-roam-capture-templates
;      '(("d" "default" plain
;         "%?" :target
;         (file+head "doc/${slug}.org" "#+title: ${title}\n")
;         :unnarrowed t)))
