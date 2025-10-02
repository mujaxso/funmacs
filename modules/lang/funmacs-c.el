;;; funmacs-c.el -*- lexical-binding: t; -*-
(use-package cc-mode
  :ensure nil
  :mode ("\\.c\\'" . c-ts-mode)
  :hook (c-ts-mode . eglot-ensure))
(provide 'funmacs-c)
