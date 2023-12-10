;;; early-init.el -*- lexical-binding: t; -*-

;;(setq gc-cons-threshold most-positive-fixnum)

;; Disable built-in package.el. Borg is used instead.
(setq package-enable-at-startup nil)

;; Let it be maximized. Surprisingly, this line in early-init.el also reduces init time.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

