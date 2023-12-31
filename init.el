;;; -*- lexical-binding: t; -*-

(setq display-line-numbers-type t)

(require 'elpaca-bootstrap)
(elpaca elpaca-use-package (elpaca-use-package-mode))
(elpaca-wait)

(use-package general
  :elpaca t)
(elpaca-wait)

;; (use-package emacs
;;   :
;;   (define-custom-command tig-menu-map)
;;   (define-custom-command tig-file-menu-map)
;;   (general-def tig-menu-map
;;     "f" '("files" . tig-file-menu-map)))

(use-package emacs
  :general
  (:prefix-command 'tig-menu-map))

(use-package emacs
  :general
  (:prefix-command 'tig-file-menu-map
		   "f" 'find-file
		   "s" 'save-buffer)
  :config
  ;; Bind prefixes after they are defined in :general section,
  ;; otherwise general creates a placeholder that hides a prefix command.
  ;; Here, we define a custom string replacement as it is recommended by which-key.
  (general-def tig-menu-map "f" '("files" . tig-file-menu-map)))

(use-package which-key
  :elpaca t
  :custom (which-key-mode t))

(use-package meow
  :elpaca t
  :demand t
  :general
  (:keymaps '(meow-normal-state-keymap
	      meow-motion-state-keymap)
	    "<menu>" 'meow-keypad
	    "SPC" 'tig-menu-map)
  (meow-motion-state-keymap
   "j" 'meow-next
   "k" 'meow-prev
   "<escape>" 'ignore)
  (mode-specific-map
   ;; SPC j/k will run the original command in MOTION state.
   "j" '"H-j"
   "k" '"H-k"
   ;; Use SPC (0-9) for digit arguments.
   "1" 'meow-digit-argument
   "2" 'meow-digit-argument
   "3" 'meow-digit-argument
   "4" 'meow-digit-argument
   "5" 'meow-digit-argument
   "6" 'meow-digit-argument
   "7" 'meow-digit-argument
   "8" 'meow-digit-argument
   "9" 'meow-digit-argument
   "0" 'meow-digit-argument
   ;; meow-keypad-describe-key doesn't work with which-key.
   "/" 'describe-key
   "?" 'meow-cheatsheet)
  (meow-normal-state-keymap
   "0" 'meow-expand-0
   "9" 'meow-expand-9
   "8" 'meow-expand-8
   "7" 'meow-expand-7
   "6" 'meow-expand-6
   "5" 'meow-expand-5
   "4" 'meow-expand-4
   "3" 'meow-expand-3
   "2" 'meow-expand-2
   "1" 'meow-expand-1
   "-" 'negative-argument
   ";" 'meow-reverse
   "," 'meow-inner-of-thing
   "." 'meow-bounds-of-thing
   "[" 'meow-beginning-of-thing
   "]" 'meow-end-of-thing
   "a" 'meow-append
   "A" 'meow-open-below
   "b" 'meow-back-word
   "B" 'meow-back-symbol
   "c" 'meow-change
   "d" 'meow-delete
   "D" 'meow-backward-delete
   "e" 'meow-next-word
   "E" 'meow-next-symbol
   "f" 'meow-find
   "g" 'meow-cancel-selection
   "G" 'meow-grab
   "h" 'meow-left
   "H" 'meow-left-expand
   "i" 'meow-insert
   "I" 'meow-open-above
   "j" 'meow-next
   "J" 'meow-next-expand
   "k" 'meow-prev
   "K" 'meow-prev-expand
   "l" 'meow-right
   "L" 'meow-right-expand
   "m" 'meow-join
   "n" 'meow-search
   "o" 'meow-block
   "O" 'meow-to-block
   "p" 'meow-yank
   "q" 'meow-quit
   "Q" 'meow-goto-line
   "r" 'meow-replace
   "R" 'meow-swap-grab
   "s" 'meow-kill
   "t" 'meow-till
   "u" 'meow-undo
   "U" 'meow-undo-in-selection
   "v" 'meow-visit
   "w" 'meow-mark-word
   "W" 'meow-mark-symbol
   "x" 'meow-line
   "X" 'meow-goto-line
   "y" 'meow-save
   "Y" 'meow-sync-grab
   "z" 'meow-pop-selection
   "'" 'repeat
   "<escape>" 'ignore)
  :custom
  (meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  :config
  ;; (load (expand-file-name "meow" user-emacs-directory))
  ;; Enable using which-key for keypad even
  ;; if which-key-mode was enabled before loading meow.
  ;; Consider contributing upstream by adding this into
  ;; meow--setup-which-key.
  (meow--which-key-describe-keymap)
  (meow-global-mode t))

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
  :general
  (:prefix-command 'tig-notes-menu-map)
  :custom
  (org-directory "~/cloud/mobile/org")
  :config
  (general-def tig-menu-map "n" '("notes" . tig-notes-menu-map)))

;; (general-define-key
;;  :keymaps 'mode-specific-map
;;  ;; :which-key "Org"
;;  :prefix-command 'my-map3
;;  :prefix-name "org"
;;  :prefix "n"
;;  :def "sad" 'my-map3)
;;  ;; "" '(nil :which-key "Org"))

;; (general-def
;;   :keymaps 'meow-normal-state-keymap
;;   "<menu>" 'meow-keypad)

;; (general-def
;;   :keymaps 'meow-normal-state-keymap
;;   :prefix "SPC"
;;   :prefix-command 'tig-normal-state-map)

;; (general-def
;;   :prefix 'tig-normal-state-map
;;   "v"
;;   :prefix-command 'tig-menu-file-map)

;; (general-def
;;   :prefix-command 'tig-normal-state-map
;;   "f s" 'find-file)

;; (general-def
;;   :prefix-map 'test-map
;;   :prefix-name "test qwe")

;; (general-def
;;   :keymaps 'mode-specific-map
;;   "v"
;;   (cons "test asd" test-map))

;; (define-key mode-specific-map "AltGr" "bar-prefix asd" 'my-map3)

;; (general-define-key
;;  ;; :keymaps 'mode-specific-map
;;  ;; :wk-full-keys nil
;;  :prefix "<menu>"
;;  :prefix-map 'mode-specific-map)
;;  ;; make a prefix-command and add description
;;  ;; "" '(:ignore t :which-key "hasd qwer"))

;; (setq my-map2 (make-sparse-keymap))
;; (define-key mode-specific-map "b" (cons "bar-prefix" my-map2))

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
  :general
  (:prefix-command 'tig-roam-menu-map
   "f" 'org-roam-node-find
   "i" 'org-roam-insert
   "r" 'org-roam-buffer-toggle)
  (:prefix-command 'tig-dailies-menu-map
		   "t" 'org-roam-dailies-goto-today
		   "m" 'org-roam-dailies-goto-tomorrow
		   "y" 'org-roam-dailies-goto-yesterday)
  :custom
  (org-roam-directory "~/cloud/mobile/org")
  (org-roam-dailies-directory "logbook")
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template
   (concat "${title:*} " (propertize "${tags}" 'face 'org-tag)))
  :config
  (advice-add 'org-roam-node-read--to-candidate
	      :override '+org-roam/format-width-a)
  (org-roam-db-autosync-mode t)
  (general-def tig-notes-menu-map "r" '("roam" . tig-roam-menu-map))
  (general-def tig-notes-menu-map "d" '("dailies" . tig-dailies-menu-map)))

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
