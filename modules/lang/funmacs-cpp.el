;;; funmacs-cpp.el -*- lexical-binding: t; -*-
(use-package cc-mode
  :ensure nil
  :mode (("\\.cpp\\'" . c++-ts-mode) ("\\.hpp\\'" . c++-ts-mode))
  :hook (c++-ts-mode . eglot-ensure))
(provide 'funmacs-cpp)
