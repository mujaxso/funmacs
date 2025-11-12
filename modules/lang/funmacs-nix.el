;;; funmacs-nix.el --- Nix configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Nix editing configuration with Tree-sitter and Eglot.

;;; Code:

(use-package nix-ts-mode
  :ensure t
  :mode ("\\.nix\\'" . nix-ts-mode)
  :hook (nix-ts-mode . (lambda ()
                         (setq-local treesit-font-lock-level 4)
                         (eglot-ensure))))

(provide 'funmacs-nix)

;;; funmacs-nix.el ends here
