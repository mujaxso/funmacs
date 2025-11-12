;;; funmacs-python.el --- Python configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Python editing configuration with Tree-sitter and Eglot.

;;; Code:

(use-package python-ts-mode
  :ensure nil
  :mode (("\\.py\\'" . python-ts-mode)
         ("\\.pyi\\'" . python-ts-mode))
  :hook (python-ts-mode . eglot-ensure))

(provide 'funmacs-python)

;;; funmacs-python.el ends here
