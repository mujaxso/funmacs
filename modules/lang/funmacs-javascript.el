;;; funmacs-javascript.el -*- lexical-binding: t; -*-

;;; Commentary:
;; javascript-mode settings

;;; code

(use-package js
  :ensure nil
  :mode ("\\.js\\'" . js-ts-mode)
  :hook (js-ts-mode . eglot-ensure))

(provide 'funmacs-javascript)

;;; funmacs-javascript.el ends here
