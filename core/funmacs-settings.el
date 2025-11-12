;;; funmacs-settings.el --- Funmacs settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic settings using use-package

;;; Code:

(use-package emacs
  :ensure nil
  :custom
  (ring-bell-function 'ignore)
  (set-default-coding-systems 'utf-8)
  (prefer-coding-system 'utf-8)
  :config
  (electric-pair-mode 1)
  (global-auto-revert-mode 1)
  (mouse-wheel-mode 1)
  (setq track-mouse t))

;; Line numbers for programming modes
(defun funmacs-enable-line-numbers ()
  "Enable relative line numbers for programming modes."
  (setq display-line-numbers-type 'relative)
  (display-line-numbers-mode 1))

(use-package prog-mode
  :ensure nil
  :hook (prog-mode . funmacs-enable-line-numbers))

;; Shell path setup
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(provide 'funmacs-settings)
;;; funmacs-settings.el ends here
