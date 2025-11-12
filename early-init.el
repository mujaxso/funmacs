;;; early-init.el --- Early Initialization -*- lexical-binding: t -*-

;;; Commentary:
;; Funmacs Maximum performance early-init Configuration
;; Loaded before GUI initialization and package system

;;; Code:

;; =============================================================================
;; GARBAGE COLLECTION OPTIMIZATION
;; =============================================================================

;; Maximize GC threshold during startup to prevent collections
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Store default file-name-handler-alist and restore after startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; =============================================================================
;; PACKAGE SYSTEM OPTIMIZATION
;; =============================================================================

;; Disable package.el at startup (we'll handle it in init.el)
(setq package-enable-at-startup nil
      package-quickstart nil)

;; Prevent premature loading of packages
(setq load-prefer-newer t)

;; =============================================================================
;; UI ELEMENT REMOVAL
;; =============================================================================

;; Disable UI elements before they're rendered (faster startup)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

;; Early GUI toggles
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Alternative method for Emacs 27+
(setq default-frame-alist
      (append (list
               '(min-height . 1)
               '(height . 45)
               '(min-width . 1)
               '(width . 81)
               '(vertical-scroll-bars . nil)
               '(horizontal-scroll-bars . nil)
               '(internal-border-width . 0)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))

;; Disable startup screens
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-scratch-message nil)

;; Window and frame settings
(setq window-divider-default-right-width 2
      window-divider-default-bottom-width 2
      window-divider-default-places t)
(window-divider-mode 1)

(modify-all-frames-parameters '((internal-border-width . 0)))

;; Fringe settings
(when (fboundp 'fringe-mode) (fringe-mode '(4 . 4)))

;; =============================================================================
;; FRAME AND WINDOW OPTIMIZATION
;; =============================================================================

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b - Emacs")
      icon-title-format frame-title-format)

(setq window-divider-default-bottom-width 1
      window-divider-default-places t
      window-divider-default-right-width 1)

;; =============================================================================
;; NATIVE COMPILATION
;; =============================================================================

(when (featurep 'native-compile)
  (setq native-comp-speed 2
        native-comp-async-report-warnings-errors 'silent
        native-comp-deferred-compilation t
        native-comp-async-jobs-number 4))

;; Suppress compiler warnings
(setq warning-suppress-types '((comp) (bytecomp)))

;; =============================================================================
;; PROCESS OPTIMIZATION
;; =============================================================================

(setq read-process-output-max (* 1024 1024)) ;; 1MB

;; =============================================================================
;; PGTK OPTIMIZATION (Wayland/Pure GTK)
;; =============================================================================

(when (featurep 'pgtk)
  (setq pgtk-wait-for-event-timeout 0.001))

;; =============================================================================
;; VERSION CONTROL OPTIMIZATION
;; =============================================================================

(defvar default-vc-handled-backends vc-handled-backends)
(setq vc-handled-backends nil)

;; =============================================================================
;; MISC STARTUP OPTIMIZATIONS
;; =============================================================================

(setq site-run-file nil)

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(setq byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent)

(setq inhibit-compacting-font-caches t)
(setq auto-mode-case-fold nil)

;; =============================================================================
;; RESTORE SETTINGS AFTER STARTUP
;; =============================================================================

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Restore garbage collection settings
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)
            
            ;; Restore file name handler
            (setq file-name-handler-alist default-file-name-handler-alist)
            
            ;; Restore version control backends
            (setq vc-handled-backends default-vc-handled-backends)
            
            ;; Garbage collect when losing focus
            (add-function :after after-focus-change-function
                          (lambda ()
                            (unless (frame-focus-state)
                              (garbage-collect))))))

(provide 'early-init)

;;; early-init.el ends here
