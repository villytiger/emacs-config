;;; -*- lexical-binding: t; -*-

(setq display-line-numbers-type t)

(require 'elpaca-bootstrap)
(elpaca elpaca-use-package (elpaca-use-package-mode))
(elpaca-wait)

(use-package general
  :elpaca t)
(elpaca-wait)

(use-package which-key
  :elpaca t
  :custom (which-key-mode t))

(use-package meow
  :elpaca t
  :after which-key
  :config
  (load (expand-file-name "meow" user-emacs-directory)))

(use-package gruvbox-theme
  :elpaca t
  :config
  (load-theme 'gruvbox t))

(use-package dashboard
 :elpaca t
 :config
 (dashboard-setup-startup-hook))

(use-package doom-modeline
  :elpaca t
  :init
  (setopt doom-modeline-mode t))

(use-package nyan-mode
  :elpaca t
  :init
  (setopt nyan-mode t))

;; (use-package shackle
;;   :config
;;   (setq shackle-rules
;; 	'(("^\\*\\([Hh]elp\\|Apropos\\)"
;; 	   :regexp t :select t)
;; 	  ("*Warnings*"
;; 	   :select t)))
;;   (shackle-mode))

(use-package helpful
  :elpaca t
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
  :elpaca t
  :init  
  (setopt marginalia-mode t))

(use-package hotfuzz
  :elpaca t
  :init
  (setopt completion-styles '(basic hotfuzz)))

(use-package icomplete
  :no-require
  :hook (emacs-startup . icomplete-mode)
  :bind
  (:map icomplete-vertical-mode-minibuffer-map
	("<return>" . icomplete-force-complete-and-exit)
	("C-<return>" . minibuffer-complete-and-exit))
  :init
  (setopt icomplete-vertical-mode t
	  icomplete-show-matches-on-no-input t
	  completion-auto-help nil))

(use-package org-mode
  :no-require
  :general (:keymaps 'mode-specific-map
	    :prefix "n"
	    :prefix-map '+org-global-map
	    :prefix-name "org")
  (:keymaps '+org-global-map
	    "f" 'org-roam-node-find)
  ;; :bind (nil
  ;; 	 :prefix-map tig/org-global-map
  ;; 	 :prefix-docstring "org"
  ;; 	 :prefix "C-c n")	       
  :custom (org-directory "~/cloud/mobile/org"))

(general-define-key
 :keymaps 'mode-specific-map
 ;; :which-key "Org"
 :prefix-command 'my-map3
 :prefix-name "org"
 :prefix "n"
 :def '("sad" . my-map3))
 ;; "" '(nil :which-key "Org"))

(general-def
  :prefix-map 'test-map
  :prefix-name "test qwe")

(general-def
  :keymaps 'mode-specific-map
  "v"
  (cons "test asd" test-map))

(define-key mode-specific-map "AltGr" '("bar-prefix asd" . my-map3))

(general-define-key
 ;; :keymaps 'mode-specific-map
 ;; :wk-full-keys nil
 :prefix "<menu>"
 :prefix-map 'mode-specific-map)
 ;; make a prefix-command and add description
 ;; "" '(:ignore t :which-key "hasd qwer"))

(setq my-map2 (make-sparse-keymap))
(define-key mode-specific-map "b" (cons "bar-prefix" my-map2))

;; (which-key-add-key-based-replacements "C-c n" "Org")

(defun +org-roam/format-width-a (node template)
  "Advice that fixes two issues with format functions:
1. They incorrectly set width for minibuffer completion.
See https://github.com/org-roam/org-roam/issues/2066.
2. When one field has '*' width and another doesn't have specified width,
the resulting string becomes wider than needed."
  (let* ((width (if (minibufferp) (window-width) (frame-width)))
	 (candidate (org-roam-node--format-entry template node width))
	 (adjustment (- width (string-width candidate)))
	 (candidate-main
	  (org-roam-node--format-entry template node (+ width adjustment))))
    (cons (propertize candidate-main 'node node) node)))

(use-package org-roam
  :elpaca t
  ;; :bind (nil
  ;; 	 :map tig/org-global-map
  ;; 	 ("f" . org-roam-node-find))
   ;; ("C-c n r i" . org-roam-insert)
   ;; ("C-c n r r" . org-roam-buffer-toggle)
   ;; ("C-c n d t" . org-roam-dailies-goto-today)
   ;; ("C-c n d m" . org-roam-dailies-goto-tomorrow)
   ;; ("C-c n d y" . org-roam-dailies-goto-yesterday))
  ;; :init
  ;; (setopt org-roam-directory "~/cloud/mobile/org"
  ;; 	  org-roam-dailies-directory "logbook"
  ;; 	  org-roam-completion-everywhere t)
  ;; (setopt org-roam-node-display-template
  ;;     (concat "${title:*} " (propertize "${tags}" 'face 'org-tag)))
  :custom
  (org-roam-directory "~/cloud/mobile/org")
  (org-roam-dailies-directory "logbook")
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template
   (concat "${title:*} " (propertize "${tags}" 'face 'org-tag)))
  :config
  (advice-add 'org-roam-node-read--to-candidate
	      :override '+org-roam/format-width-a)
  (org-roam-db-autosync-mode t))

(use-package consult-org-roam
  :elpaca t
  :after org-roam
  ;; :bind
  ;; (("C-c n r e" . consult-org-roam-file-find))
  :init
  ;; It enables live preview for org-roam commands.
  (setopt consult-org-roam-mode t))

(use-package edebug
  :no-require
  :bind
  ;; Default key binding using SPC is not compatible with Meow.
  (:map edebug-mode-map ("s" . edebug-step-mode)))

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
