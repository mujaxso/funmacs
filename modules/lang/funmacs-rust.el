;;; funmacs-rust.el --- Rust configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Rust editing configuration with Tree-sitter and Eglot.

;;; Code:

(use-package rust-ts-mode
  :ensure nil
  :mode ("\\.rs\\'" . rust-ts-mode)
  :hook (rust-ts-mode . eglot-ensure))

(provide 'funmacs-rust)

;;; funmacs-rust.el ends here
