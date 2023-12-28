;;; early-init.el -*- lexical-binding: t; -*-

;; Disable GC while starting up.
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Disable built-in package.el.
(setq package-enable-at-startup nil)

(setcar native-comp-eln-load-path
(expand-file-name ".local/cache/eln" user-emacs-directory))

(setq inhibit-startup-screen t)

;; Let it be maximized.
(setq initial-frame-alist '((fullscreen . maximized)))

;; No menu bar, toolbar, scroll bar.
(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (horizontal-scroll-bars)
        (vertical-scroll-bars)))

(set-face-attribute 'default nil :font "Fira Code")

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (require 'elpaca-bootstrap)
;; (require 'tig-lib)
