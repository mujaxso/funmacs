;;; funmacs-rust.el -*- lexical-binding: t; -*-
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-hook 'rust-ts-mode-hook #'eglot-ensure)
(provide 'funmacs-rust)
