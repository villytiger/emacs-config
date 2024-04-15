;; Garbage Collection  -*- "lexical-binding": t; -*-

;; We effectively disable garbage collection while Emacs is starting up by setting the threshold to 1 GiB. This famous hack cuts startup time in half. This section should be located as high as possible in the file so that other commands do not trigger garbage collection. Later on, ~gcmh~ will re-enable garbage collection in idle periods.


;; [[file:config.org::*Garbage Collection][Garbage Collection:1]]
(setq gc-cons-threshold #x40000000)
;; Garbage Collection:1 ends here

;; The rest


;; [[file:config.org::*The rest][The rest:1]]
;; Disable built-in package.el.
;; (setq package-enable-at-startup nil)

(setcar native-comp-eln-load-path
        (expand-file-name ".data/cache/eln" user-emacs-directory))

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
;; The rest:1 ends here
