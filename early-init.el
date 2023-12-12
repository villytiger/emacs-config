;;; early-init.el -*- lexical-binding: t; -*-

;; Disable GC while starting up.
(setq vt-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)

;; Disable built-in package.el in favor of Borg.
(setq package-enable-at-startup nil)

;; Let it be maximized.
(setq initial-frame-alist '((fullscreen . maximized)))

;; no menu bar, toolbar, scroll bar
(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (horizontal-scroll-bars)
        (vertical-scroll-bars)))

(setq inhibit-startup-message t)

(set-face-attribute 'default nil :font "Fira Code")
