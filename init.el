;; Literate Config Goodies  -*- "lexical-binding": t; -*-

;; TODO move to use-package for org-mode or org-babel


;; [[file:config.org::*Literate Config Goodies][Literate Config Goodies:1]]
(defun arete--post-tangle ()
  (message "adjusting tangled file")
  (cond
   ((f-ext? (buffer-file-name) "el")
    (add-file-local-variable-prop-line "lexical-binding" t)))
  (save-buffer))
;; (defun arete--literate-init ()
  ;; (make-local-variable 'org-babel-post-tangle-hook)
;; (add-hook 'org-babel-post-tangle-hook 'arete--literate-post-tangle nil t)
(add-hook 'org-babel-post-tangle-hook 'arete--post-tangle)
  ;; )
;; (put 'arete--literate-init 'safe-local-eval-function t)
;; Literate Config Goodies:1 ends here

;; Use-Package


;; [[file:config.org::*Use-Package][Use-Package:1]]
(setopt use-package-enable-imenu-support t)
;; Use-Package:1 ends here



;; Tell ~use-package~ to always load features lazily unless told otherwise. It's nicer to have this kind of thing be deterministic: if ~:demand~ is present, the loading is eager; otherwise, the loading is lazy. See https://github.com/jwiegley/use-package#notes-about-lazy-loading.


;; [[file:config.org::*Use-Package][Use-Package:2]]
(setopt use-package-always-defer t)
;; Use-Package:2 ends here

;; [[file:config.org::*Use-Package][Use-Package:3]]
(setopt use-package-always-ensure t)
;; Use-Package:3 ends here



;; Built-in features should be configured with ~use-feature~, so that ~Elpaca~ wouldn't bother to install them.


;; [[file:config.org::*Use-Package][Use-Package:4]]
(defmacro use-feature (name &rest args)
  "`use-package' that makes sure `ensure' is disabled."
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     ,@args))
;; Use-Package:4 ends here

;; Bootstrap


;; [[file:config.org::*Bootstrap][Bootstrap:1]]
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;; Bootstrap:1 ends here

;; Elpaca Configuration


;; [[file:config.org::*Elpaca Configuration][Elpaca Configuration:1]]
(elpaca elpaca-use-package (elpaca-use-package-mode))
(elpaca-wait)
;; Elpaca Configuration:1 ends here

;; Garbage Collection Magic Hack


;; [[file:config.org::*Garbage Collection Magic Hack][Garbage Collection Magic Hack:1]]
(use-package gcmh
  :custom
  (gcmh-mode t))
;; Garbage Collection Magic Hack:1 ends here

;; All the rest


;; [[file:config.org::*All the rest][All the rest:1]]
(setopt
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 column-number-mode t
 blink-cursor-mode nil
 indent-tabs-mode nil)

(setopt
 ;; Explicitly define the minimal width to reduce the cost of on-the-fly computation.
 display-line-numbers-width 3
 ;; Show absolute line numbers for narrowed regions to make it easier to tell the
 ;; buffer is narrowed, and where you are, exactly.
 display-line-numbers-widen t)

(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; All the rest:1 ends here

;; [[file:config.org::*All the rest][All the rest:2]]
(use-package general :demand t)
(elpaca-wait)

(use-feature emacs
  :general
  (:prefix-command 'arete-menu-map))

(use-feature emacs
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

(use-feature emacs
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

(use-feature emacs
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
  :custom (which-key-mode t))

(use-package meow
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

;; Remove when gruvbox is installed via Elpaca.
(use-package autothemer :demand t)
(use-package fontify-face)
(use-package rainbow-mode)

(use-package gruvbox-theme
  :after autothemer
  :demand t
  :load-path "packages/emacs-theme-gruvbox"
  :config
  (load-theme 'gruvbox t))

(use-feature emacs
  :config
  (custom-set-faces
   '(line-number ((t :weight light)))
   '(line-number-current-line ((t :weight light)))))

(use-package solaire-mode
  :custom
  (solaire-global-mode t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dashboard
  :init
  (dashboard-setup-startup-hook))

(use-package doom-modeline
  :custom
  (doom-modeline-mode t))

(use-package nyan-mode
  :custom
  (nyan-mode t))

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

(use-feature savehist
  :no-require
  :custom
  (savehist-mode t))

(use-package marginalia
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
  :custom
  (vertico-mode t))

(use-package corfu
  :general
  (corfu-map
   "<escape>" 'corfu-reset
   "M-<escape>" 'corfu-quit)
  :custom
  (global-corfu-mode t)
  (tab-always-indent 'complete))

;; TODO: embark-consult.
(use-package embark
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
  :general
  ([remap recentf-open] #'consult-recent-file))

(use-feature edebug
  :no-require
  :general
  ;; Default key binding uses SPC.
  (edebug-mode-map "s" 'edebug-step-mode))
;; All the rest:2 ends here

;; Org Mode


;; [[file:config.org::*Org Mode][Org Mode:1]]
(use-feature org
  :hook (org-mode . visual-line-mode)
  :general
  (:prefix-command 'arete-notes-menu-map
                   "a" '("Agenda" . org-agenda))
  :custom
  (org-directory "~/cloud/mobile/org")
  (org-support-shift-select t)
  (org-startup-indented t)
  (org-catch-invisible-edits 'show-and-error)
  ;; hide the emphasis markup (e.g. /.../ for italics, *...* for bold, etc.)
  (org-hide-emphasis-markers t)
  ;; formats sub- and superscripts in a WYSIWYM way
  (org-pretty-entities t)
  ;; uses to indicate hidden content
  (org-ellipsis "…")
  :config
  (general-def arete-menu-map "n" '("Notes" . arete-notes-menu-map)))
;; Org Mode:1 ends here



;; Inline tasks are disabled by default, although they seem very useful for quickly defining small tasks without introducing a first-class header. Technically, they are defined as headers, but deeply nested. Try out by running ~org-inlinetask-insert-task~ on an empty line.


;; [[file:config.org::*Org Mode][Org Mode:2]]
(use-feature org-inlinetask :demand t)
;; Org Mode:2 ends here

;; Org Modern


;; [[file:config.org::*Org Modern][Org Modern:1]]
(use-package org-modern
  :after org
  :custom
  (org-modern-block-name '(("src" "λ" "λ")))
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-star '("🞴" "🞳" "🞲" "🞱" "🞰"))
  ;; modern tags are auto-misaligned
  (org-auto-align-tags nil)
  (org-tags-column 0)
  :config
  ;; The default face reduces the size of block names,
  ;; but we want the whole block line to be smaller than normal lines,
  ;; so without this setting block names would be twice smaller.
  (face-spec-set 'org-modern-block-name nil 'face-defface-spec)
  (face-spec-set 'org-modern-done
                 '((t :inherit (org-done org-modern-label) :inverse-video t))
                 'face-defface-spec)
  (face-spec-set 'org-modern-tag
                 '((t :inherit (org-tag org-modern-label) :inverse-video t))
                 'face-defface-spec))
;; Org Modern:1 ends here

;; Org Modern Indent


;; [[file:config.org::*Org Modern Indent][Org Modern Indent:1]]
(use-package org-modern-indent
  :ensure (:host github :repo "jdtsmith/org-modern-indent")
  :after org
  :hook ('org-mode . org-modern-indent-mode))
;; Org Modern Indent:1 ends here

;; Org Roam


;; [[file:config.org::*Org Roam][Org Roam:1]]
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
;; Org Roam:1 ends here

;; Consult Org Roam

;; Enable live preview for org-roam commands.

;; *************** TODO Explore other consult-org-roam options


;; [[file:config.org::*Consult Org Roam][Consult Org Roam:1]]
(use-package consult-org-roam
  :after org-roam
  :custom
  (consult-org-roam-mode t))
;; Consult Org Roam:1 ends here
