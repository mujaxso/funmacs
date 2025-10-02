;;; funmacs-theme.el --- theme -*- lexical-binding: t; -*-
(use-package modus-themes
  :init (setq modus-themes-italic-constructs t)
  :config (load-theme 'modus-vivendi-tinted t))
(provide 'funmacs-theme)
