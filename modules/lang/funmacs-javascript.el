;;; funmacs-javascript.el --- JavaScript configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; JavaScript editing configuration with Tree-sitter and Eglot.

;;; Code:

(use-package js-ts-mode
  :ensure nil
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.mjs\\'" . js-ts-mode))
  :hook (js-ts-mode . eglot-ensure))

(provide 'funmacs-javascript)

;;; funmacs-javascript.el ends here
