;;; init.el -*- lexical-binding: t; -*-

;(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;(setq display-line-numbers-type t)

;(use-package borg
;  :demand t
;  :load-path (lambda () (expand-file-name "lib/borg" user-emacs-directory))
;  :config
;  (when (native-comp-available-p)
;    (setq borg-compile-function #'borg-byte+native-compile))
  ;; Override borg-activate to disable autoloading.
  ;; Don't clutter emacs with unused packages.
  ;; Let use-package do its job.
;;   (defun borg-activate (clone)
;;     "Activate the clone named CLONE.

;; Add the appropriate directories to `load-path' and
;; `Info-directory-list'."
;;     (interactive (list (borg-read-clone "Activate clone: ")))
;;     ;; (dolist (dir (borg-load-path clone))
;;       ;; (add-to-list 'load-path dir))
;;     (dolist (dir (borg-info-path clone))
;;       (push  dir Info-directory-list)))
;);  (borg-initialize))

;; (setq init-dir (file-name-directory (or load-file-name (buffer-file-name))))
;; (defun load-from-init-dir (local-path)
;;   (load (expand-file-name local-path init-dir)))

;; (load-from-init-dir "elpaca")

;; (use-package gruvbox-theme :config (load-theme 'gruvbox t))
;; (use-package meow :config (load (expand-file-name "meow" user-emacs-directory)))

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
