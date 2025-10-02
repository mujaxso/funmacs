;;; funmacs-c.el -*- lexical-binding: t; -*-

;;; Commentary:
;; c-mode settings

;;; code

(use-package cc-mode
  :ensure nil
  :mode ("\\.c\\'" . c-ts-mode)
  :hook (c-ts-mode . eglot-ensure))

(provide 'funmacs-c)

;;; funmacs-c.el ends here
