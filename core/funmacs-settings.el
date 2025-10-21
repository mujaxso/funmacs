;;; funmacs-settings.el --- Funmacs settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic settings: disable bell sound and enable line numbers.

;;; Code:

;; Disable bell sound
(setq ring-bell-function 'ignore)

;; Enable relative line numbers in programming modes only
(defun funmacs-enable-line-numbers ()
  "Enable relative line numbers for programming modes."
  (setq display-line-numbers-type 'relative)
  (display-line-numbers-mode 1))

(add-hook 'prog-mode-hook #'funmacs-enable-line-numbers)

;; autocomplete paired brackets
(electric-pair-mode 1)

(provide 'funmacs-settings)
;;; funmacs-settings.el ends here
