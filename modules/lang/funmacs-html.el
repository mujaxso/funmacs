;;; funmacs-html.el --- HTML configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; HTML editing configuration with Tree-sitter and Eglot.

;;; Code:

(use-package html-ts-mode
  :ensure nil
  :mode ("\\.html?\\'" . html-ts-mode)
  :hook (html-ts-mode . eglot-ensure))

(provide 'funmacs-html)

;;; funmacs-html.el ends here
