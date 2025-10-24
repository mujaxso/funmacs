;;; early-init.el --- Early Initialization -*- lexical-binding: t -*-

;;; Commentary:
;; Funmacs Maximum performance early-init Configuration
;; Loaded before GUI initialization and package system

;;; Code:

;; =============================================================================
;; GARBAGE COLLECTION OPTIMIZATION
;; =============================================================================

;; Maximize GC threshold during startup to prevent collections
;; This can reduce startup time by 50% or more
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

;; Hairline dividers, theme-colored, no extra padding
(setq window-divider-default-right-width 2
      window-divider-default-bottom-width 2
      window-divider-default-places t)
(window-divider-mode 1)

;; No frame padding for maximum content area
(modify-all-frames-parameters '((internal-border-width . 0)))

;; Slim, symmetric fringes for minimal gutters
(fringe-mode '(4 . 4))

;; =============================================================================
;; FRAME AND WINDOW OPTIMIZATION
;; =============================================================================

;; Prevent frame resizing when adjusting fonts
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b - Emacs")
      icon-title-format frame-title-format)

;; Window divider settings
(setq window-divider-default-bottom-width 1
      window-divider-default-places t
      window-divider-default-right-width 1)

;; =============================================================================
;; NATIVE COMPILATION
;; =============================================================================

;; Native compilation settings (Emacs 28+)
(when (featurep 'native-compile)
  (setq native-comp-speed 2
        native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t
        native-comp-async-jobs-number 4))

;; =============================================================================
;; PROCESS OPTIMIZATION
;; =============================================================================

;; Increase the amount of data read from processes
(setq read-process-output-max (* 1024 1024)) ;; 1MB

;; =============================================================================
;; PGTK OPTIMIZATION (Wayland/Pure GTK)
;; =============================================================================

;; Reduce pgtk timeout for better performance on Wayland
(when (featurep 'pgtk)
  (setq pgtk-wait-for-event-timeout 0.001))

;; =============================================================================
;; VERSION CONTROL OPTIMIZATION
;; =============================================================================

;; Disable version control during startup
(defvar default-vc-handled-backends vc-handled-backends)
(setq vc-handled-backends nil)

;; =============================================================================
;; MISC STARTUP OPTIMIZATIONS
;; =============================================================================

;; Disable site-run-file
(setq site-run-file nil)

;; Disable bidirectional text rendering for slight performance boost
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disable warnings and reduce noise
(setq byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      native-comp-async-report-warnings-errors 'silent)

;; Faster font rendering
(setq inhibit-compacting-font-caches t)

;; Disable automatic file handler during startup
(setq auto-mode-case-fold nil)

;; =============================================================================
;; RESTORE SETTINGS AFTER STARTUP
;; =============================================================================

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Restore garbage collection settings (16MB threshold)
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)
            
            ;; Restore file name handler
            (setq file-name-handler-alist default-file-name-handler-alist)
            
            ;; Restore version control backends
            (setq vc-handled-backends default-vc-handled-backends)
            
            ;; Garbage collect when losing focus (optional but useful)
            (add-function :after after-focus-change-function
                          (lambda ()
                            (unless (frame-focus-state)
                              (garbage-collect))))
            
            ;; ;; Display startup time
            ;; (message "Emacs loaded in %s with %d garbage collections."
            ;;          (emacs-init-time "%.2f seconds")
            ;;          gcs-done)
	    ))

(provide 'early-init)

;;; early-init.el ends here
