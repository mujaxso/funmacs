;;; funmacs-nix.el -*- lexical-binding: t; -*-
(use-package nix-mode :ensure t :mode ("\\.nix\\'" . nix-mode))
(add-hook 'nix-mode-hook #'eglot-ensure)
(provide 'funmacs-nix)
