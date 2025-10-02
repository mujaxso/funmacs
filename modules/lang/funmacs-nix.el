;;; funmacs-nix.el -*- lexical-binding: t; -*-

;;; Commentary:
;; nix-mode settings

;;; code

(use-package nix-mode
  :ensure t
  :mode ("\\.nix\\'" . nix-mode))
(add-hook 'nix-mode-hook #'eglot-ensure)

(declare-function nix-read-attr "ext:nix-mode")
(declare-function nix-read-file "ext:nix-mode")

(provide 'funmacs-nix)

;;; funmacs-nix.el ends here
