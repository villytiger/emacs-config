;; Garbage Collection  -*- "lexical-binding": t; -*-

;; We effectively disable garbage collection while Emacs is starting up by setting the threshold to 1 GiB. This famous hack cuts startup time in half. This section should be located as high as possible in the file so that other commands do not trigger garbage collection. Later on, ~gcmh~ will re-enable garbage collection in idle periods.


;; [[file:config.org::*Garbage Collection][Garbage Collection:1]]
(setq gc-cons-threshold #x40000000)
;; Garbage Collection:1 ends here

;; Constants


;; [[file:config.org::*Constants][Constants:1]]
(defconst xdg-config-home
  (or (when-let ((dir (getenv "XDG_CONFIG_HOME")))
        (file-name-as-directory dir))
      (when-let ((dir (getenv "HOME")))
        (expand-file-name ".config/" dir))
      (error "Cannot detect configuration directory")))

(defconst arete-config-dir
  (expand-file-name "arete/" xdg-config-home)
  "The directory that contains local configuration
where Arete can be extended and variables can be overriden.
This is a separate repository not integrated with Arete.")

(defconst arete-local-dir
  (expand-file-name ".local/" user-emacs-directory)
  "The directory that contains all local automatically created files.")

(defconst arete-cache-dir
  (expand-file-name "cache" arete-local-dir))
;; Constants:1 ends here

;; Options


;; [[file:config.org::*Options][Options:1]]
(setopt native-comp-async-report-warnings-errors 'silent)
;; Options:1 ends here

;; The rest


;; [[file:config.org::*The rest][The rest:1]]
(startup-redirect-eln-cache
 (expand-file-name "eln" arete-cache-dir))

(setq inhibit-startup-screen t)

;; Let it be maximized.
(setq initial-frame-alist '((fullscreen . maximized)))

;; No menu bar, toolbar, scroll bar.
(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (horizontal-scroll-bars)
        (vertical-scroll-bars)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; The rest:1 ends here
