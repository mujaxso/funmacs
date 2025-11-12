;;; funmacs-react.el --- React configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; React editing configuration with Tree-sitter and Eglot.

;;; Code:

(use-package tsx-ts-mode
  :ensure nil
  :mode ("\\.tsx\\'" . tsx-ts-mode)
  :hook (tsx-ts-mode . eglot-ensure))

(provide 'funmacs-react)

;;; funmacs-react.el ends here
