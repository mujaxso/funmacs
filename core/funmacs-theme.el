;;; funmacs-theme.el --- theme -*- lexical-binding: t; -*-

;;; Commentary:
;; themes settings

;;;code

(use-package modus-themes
  :demand t
  :init (setq modus-themes-italic-constructs t)
  :config (load-theme 'modus-vivendi-tinted t))

(provide 'funmacs-theme)

;;; funmacs-theme.el ends here
