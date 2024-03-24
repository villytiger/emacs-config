;; Early Configuration  -*- "lexical-binding": t; -*-
;; :PROPERTIES:
;; :header-args:emacs-lisp+: :tangle "early-init.el"
;; :END:

;; We put some configuration options into early-init.el.


;; [[file:config.org::*Early Configuration][Early Configuration:1]]
;; Disable GC while starting up.
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

;; Disable built-in package.el.
(setq package-enable-at-startup nil)

(setopt use-package-enable-imenu-support t)

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
(set-face-attribute 'fixed-pitch nil :font "Fira Code")
(set-face-attribute 'variable-pitch nil :font "Fira Sans")
(set-fontset-font t 'symbol "Noto Color Emoji")
(set-fontset-font t 'symbol "Symbola" nil 'append)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (require 'elpaca-bootstrap)
;; (require 'tig-lib)
;; Early Configuration:1 ends here
