;;; -*- lexical-binding: t; -*-

;; (setq display-line-numbers-type t)

(setopt
 completion-ignore-case t
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 column-number-mode t
 blink-cursor-mode nil)

(setopt
 ;; Explicitly define the minimal width to reduce the cost of on-the-fly computation.
 display-line-numbers-width 3
 ;; Show absolute line numbers for narrowed regions to make it easier to tell the
 ;; buffer is narrowed, and where you are, exactly.
 display-line-numbers-widen t)

(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(require 'elpaca-bootstrap)
(elpaca elpaca-use-package (elpaca-use-package-mode))
(elpaca-wait)

(use-package general
  :elpaca t)
(elpaca-wait)

(use-package emacs
  :general
  (:prefix-command 'arete-menu-map))

(use-package emacs
  :general
  (:prefix-command 'arete-buffer-menu-map
		   "R" '("Rename buffer" . rename-buffer)
		   "S" '("Save some buffers" . save-some-buffers)
		   ;; "X" '("Scratch buffer" . )
		   "[" '("Previous buffer" . previous-buffer)
		   "]" '("Next buffer" . next-buffer)
		   "b" '("Switch buffer" . switch-to-buffer)
		   "d" '("Kill buffer" . kill-buffer)
		   "l" '("Last buffer" . mode-line-other-buffer)
		   ;; "n" '("New buffer" . )
		   "r" '("Revert buffer" . revert-buffer)
		   "s" '("Save buffer" . basic-save-buffer))
  :config
  (general-def arete-menu-map "b" '("Buffers" . arete-buffer-menu-map)))

(use-package emacs
  :general
  (:prefix-command 'arete-file-menu-map
		   "f" '("Find file" . find-file)
		   "r" '("Recent files" . recentf-open)
		   "s" '("Save file" . save-buffer)
		   "S" '("Save file as..." . write-file))
  :config
  ;; Bind prefixes after they are defined in :general section,
  ;; otherwise general creates a placeholder that hides a prefix command.
  ;; Here, we define a custom string replacement as it is recommended by which-key.
  (general-def arete-menu-map "f" '("Files" . arete-file-menu-map)))

(use-package emacs
  :general
  (:prefix-command 'arete-help-menu-map
		   "B" '("Describe bindings" . describe-bindings)
   ;; Note that the built-in `describe-function' includes both functions
   ;; and macros. `helpful-function' is functions only, so we provide
   ;; `helpful-callable' as a drop-in replacement.
   "f" '("Describe callable" . describe-function)
   "k" '("Describe key" . describe-key)
   "o" '("Describe symbol" . describe-symbol)
   "v" '("Describe variable" . describe-variable)
   "x" '("Describe command" . describe-command))
  :config
  (general-def arete-menu-map "h" '("Help" . arete-help-menu-map)))

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
            "SPC" 'arete-menu-map)
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

;; (use-package gruvbox-theme
;;   :elpaca t
;;   :config
;;   (load-theme 'gruvbox t)
;;   ;; autothemer-let-palette gets palette from the last loaded/evaled theme.
;;   ;; So this block must be executed immediately after loading theme, but for
;;   ;; some reason patching works only after enabling.
;;   ;; TODO: Figure out why enabling is needed.
;;   (autothemer-let-palette
;;    (custom-theme-set-faces
;;     'gruvbox
;;     `(line-number
;;       ((t :background ,gruvbox-dark0_hard
;; 	  :foreground ,gruvbox-dark3)))
;;     `(line-number-current-line
;;       ((t :background ,gruvbox-dark0_hard
;; 	  :foreground ,gruvbox-faded_yellow)))
;;     `(solaire-default-face
;;       ((t :background ,gruvbox-dark0_hard)))
;;     `(solaire-minibuffer-face
;;       ((t :background ,gruvbox-dark0_hard)))
;;     `(solaire-hl-line-face
;;       ((t :background ,gruvbox-dark0_hard)))
;;     `(solaire-org-hide-face
;;       ((t :background ,gruvbox-dark0_hard)))))
;;   ;; Theme must be enabled again for modifications to work.
;;   (enable-theme 'gruvbox))

(use-package autothemer
  :elpaca t)

(use-package fontify-face
  :elpaca t)

(use-package gruvbox-theme
  :after autothemer
  :load-path "packages/emacs-theme-gruvbox"
  :config
  (load-theme 'gruvbox t))

(use-package emacs
  :config
  (custom-set-faces
   '(line-number ((t :weight light)))
   '(line-number-current-line ((t :weight light)))))

(use-package solaire-mode
  :elpaca t
  :custom
  (solaire-global-mode t))

(use-package rainbow-delimiters
  :elpaca t
  :hook (prog-mode . rainbow-delimiters-mode))

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

;; TODO: add go-back and go-forward.
;; See https://github.com/Wilfred/helpful/issues/250.
(use-package helpful
  :elpaca t
  :general
  (arete-help-menu-map
   "F" '("Describe function" . helpful-function)
   "d" '("Describe at point" . helpful-at-point))
  ([remap describe-command] #'helpful-command
   ;; Note that the built-in `describe-function' includes both functions
   ;; and macros. `helpful-function' is functions only, so we provide
   ;; `helpful-callable' as a drop-in replacement.
   [remap describe-function] #'helpful-callable
   [remap describe-key] #'helpful-key
   [remap describe-symbol] #'helpful-symbol
   [remap describe-variable] #'helpful-variable)
  :custom
  (helpful-switch-buffer-function #'+helpful-switch-to-buffer)
  :config
  ;; https://d12frosted.io/posts/2019-06-26-emacs-helpful.html
  (defun +helpful-switch-to-buffer (buffer-or-name)
  "Switch to helpful BUFFER-OR-NAME.

The logic is simple, if we are currently in the helpful buffer,
reuse it's window, otherwise create new one."
  (if (eq major-mode 'helpful-mode)
      (switch-to-buffer buffer-or-name)
    (pop-to-buffer buffer-or-name))))

(use-package savehist
  :no-require
  :custom
  (savehist-mode t))

(use-package marginalia
  :elpaca t
  :custom
  (marginalia-mode t)
  :config
  ;; https://github.com/minad/marginalia/issues/155
  ;; https://github.com/minad/marginalia/tree/mode-state
  (defun +marginalia--mode-state (mode)
    "Return MODE state string."
    (if (and (boundp mode) (symbol-value mode))
        #(" [On]" 1 5 (face marginalia-key))
      #(" [Off]" 1 6 (face marginalia-key))))
  (defun +marginalia-annotate-command-with-mode (orig cand)
    "Annotate command CAND with its documentation string.
Similar to `marginalia-annotate-command`, but also includes mode state."
    (concat
     (when-let ((mode (string-suffix-p "-mode" cand))
		(sym (intern-soft cand)))
       (+marginalia--mode-state sym))
     (funcall orig cand)))
  (advice-add #'marginalia-annotate-command
	      :around #'+marginalia-annotate-command-with-mode))

(use-package hotfuzz
  :elpaca t
  :general
  (vertico-map
   "SPC" 'minibuffer-complete-word)
  :custom
  ;; Some functionality works only with basic completion.
  ;; Basic should go first, otherwise history doesn't work.
  (completion-styles '(hotfuzz basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles basic partial-completion hotfuzz))))
  :config
  (defvar +hotfuzz--is-empty)
  (defun +hotfuzz-all-completions--enable-history-a (orig content &rest args)
    "Set a variable needed for showing most recent entries."
    (setq +hotfuzz--is-empty (string-empty-p content))
    (apply orig content args))
  (advice-add #'hotfuzz-all-completions
	      :around #'+hotfuzz-all-completions--enable-history-a)
  (defun +hotfuzz--adjust-metadata--enable-history-a (orig metadata)
    "Enable showing most recent entries for empty input."
    (if +hotfuzz--is-empty
	metadata
	(funcall orig metadata)))
  (advice-add #'hotfuzz--adjust-metadata
	      :around #'+hotfuzz--adjust-metadata--enable-history-a))

(use-package vertico
  :elpaca t
  :custom
  (vertico-mode t))

(use-package corfu
  :elpaca t
  :general
  (corfu-map
   "<escape>" 'corfu-reset
   "M-<escape>" 'corfu-quit)
  :custom
  (global-corfu-mode t)
  (tab-always-indent 'complete))

;; TODO: embark-consult.
(use-package embark
  :elpaca t
  :general
  ("M-SPC" 'embark-act)
  (arete-help-menu-map
   "b" '("Select biniding" . embark-bindings))
  :custom
  (prefix-help-command 'embark-prefix-help-command))

;; (use-package icomplete
;;   :no-require
;;   :hook (emacs-startup . icomplete-mode)
;;   :bind
;;   (:map icomplete-vertical-mode-minibuffer-map
;; 	("<return>" . icomplete-force-complete-and-exit)
;; 	("C-<return>" . minibuffer-complete-and-exit))
;;   :init
;;   (setopt icomplete-vertical-mode t
;; 	  icomplete-show-matches-on-no-input t
;; 	  completion-auto-help nil))

(use-package consult
  :elpaca t
  :general
  ([remap recentf-open] #'consult-recent-file))

(use-package org
  :no-require
  :general
  (:prefix-command 'arete-notes-menu-map
		   "a" '("Agenda" . org-agenda))
  :custom
  (org-directory "~/cloud/mobile/org")
  :config
  (general-def arete-menu-map "n" '("Notes" . arete-notes-menu-map)))

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
  :after org
  :general
  (:prefix-command 'arete-roam-menu-map
   "f" '("Find node" . org-roam-node-find)
   "i" '("Insert node" . org-roam-insert)
   "r" '("Toggle roam buffer" . org-roam-buffer-toggle))
  (:prefix-command 'arete-dailies-menu-map
		   "t" '("Goto today" . org-roam-dailies-goto-today)
		   "m" '("Goto tomorrow" . org-roam-dailies-goto-tomorrow)
		   "y" '("Goto yesterday" . org-roam-dailies-goto-yesterday))
  :custom
  (org-roam-directory "~/cloud/mobile/org")
  (org-roam-dailies-directory "logbook")
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template
   (concat "${title:*} " (propertize "${tags}" 'face 'org-tag)))
  :init
  (general-def arete-notes-menu-map "r" '("Roam" . arete-roam-menu-map))
  (general-def arete-notes-menu-map "d" '("Dailies" . arete-dailies-menu-map))
  :config
  (advice-add 'org-roam-node-read--to-candidate
	      :override '+org-roam/format-width-a)
  (org-roam-db-autosync-mode t))

(use-package consult-org-roam
  :elpaca t
  :after org-roam
  :init
  ;; It enables live preview for org-roam commands.
  (setopt consult-org-roam-mode t))

(use-package edebug
  :no-require
  :general
  ;; Default key binding uses SPC.
  (edebug-mode-map "s" 'edebug-step-mode))
