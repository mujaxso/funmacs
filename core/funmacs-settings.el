;;; funmacs-settings.el --- Minimal core settings for Funmacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic settings: disable bell sound and enable line numbers.

;;; Code:

;; Disable bell sound
(setq ring-bell-function 'ignore)

;; Enable global line numbers
(global-display-line-numbers-mode t)

(provide 'funmacs-settings)
;;; funmacs-settings.el ends here
