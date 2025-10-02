;;; funmacs-css.el -*- lexical-binding: t; -*-
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
(add-hook 'css-ts-mode-hook #'eglot-ensure)
(provide 'funmacs-css)
