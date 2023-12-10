;;; early-init.el -*- lexical-binding: t; -*-

;; Disable GC while starting up.
(setq vt-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)

;; Disable built-in package.el in favor of Borg.
(setq package-enable-at-startup nil)

;; Let it be maximized.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
