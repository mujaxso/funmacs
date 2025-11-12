;;; funmacs-json.el --- JSON configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; JSON editing configuration with Tree-sitter and Eglot.

;;; Code:

(use-package json-ts-mode
  :ensure nil
  :mode ("\\.json\\'" . json-ts-mode)
  :hook (json-ts-mode . eglot-ensure))

(provide 'funmacs-json)

;;; funmacs-json.el ends here
