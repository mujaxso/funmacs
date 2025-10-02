;;; funmacs-go.el -*- lexical-binding: t; -*-
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-hook 'go-ts-mode-hook #'eglot-ensure)
(provide 'funmacs-go)
