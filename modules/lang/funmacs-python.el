;;; funmacs-python.el -*- lexical-binding: t; -*-

;;; Commentary:
;; python-mode settings

;;; code

(use-package python
  :ensure nil
  :mode (("\\.py\\'" . python-ts-mode))
  :hook (python-ts-mode . eglot-ensure))

(provide 'funmacs-python)

;;; funmacs-python.el ends here
