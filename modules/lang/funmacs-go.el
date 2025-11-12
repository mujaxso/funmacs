;;; funmacs-go.el --- Go configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Go editing configuration with Tree-sitter and Eglot.

;;; Code:

(use-package go-ts-mode
  :ensure nil
  :mode ("\\.go\\'" . go-ts-mode)
  :hook (go-ts-mode . eglot-ensure))

(provide 'funmacs-go)

;;; funmacs-go.el ends here
