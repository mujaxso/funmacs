;;; funmacs-cpp.el -*- lexical-binding: t; -*-

;;; Commentary:
;; cpp-mode settings

;;; code

(use-package cc-mode
  :ensure nil
  :mode (("\\.cpp\\'" . c++-ts-mode) ("\\.hpp\\'" . c++-ts-mode))
  :hook (c++-ts-mode . eglot-ensure))

(provide 'funmacs-cpp)

;;; funmacs-cpp.el ends here
