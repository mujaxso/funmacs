;;; funmacs-settings.el --- Funmacs settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic settings: disable bell sound and enable line numbers.

;;; Code:

;; Disable bell sound
(setq ring-bell-function 'ignore)

;; Enable relative line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

(provide 'funmacs-settings)
;;; funmacs-settings.el ends here
