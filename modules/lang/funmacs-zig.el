;;; funmacs-zig.el -*- lexical-binding: t; -*-

;;; Commentary:
;; zig-mode settings

;;; code

(use-package zig-mode
  :ensure t
  :mode ("\\.zig\\'" . zig-mode)
  :hook (zig-mode . eglot-ensure))

(provide 'funmacs-zig)

;;; funmacs-zig.el ends here
