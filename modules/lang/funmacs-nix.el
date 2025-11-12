;;; funmacs-nix.el --- Nix configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Nix editing configuration with Tree-sitter and Eglot.

;;; Code:

(use-package nix-ts-mode
  :ensure nil
  :mode ("\\.nix\\'" . nix-ts-mode)
  :hook (nix-ts-mode . eglot-ensure))

(provide 'funmacs-nix)

;;; funmacs-nix.el ends here
