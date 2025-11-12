;;; funmacs-css.el --- CSS configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; CSS editing configuration with Tree-sitter and Eglot.

;;; Code:

(use-package css-ts-mode
  :ensure nil
  :mode ("\\.css\\'" . css-ts-mode)
  :hook (css-ts-mode . eglot-ensure))

(provide 'funmacs-css)

;;; funmacs-css.el ends here
