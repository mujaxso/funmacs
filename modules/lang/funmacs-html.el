;;; funmacs-html.el -*- lexical-binding: t; -*-
(add-to-list 'auto-mode-alist '("\\.html?\\'" . html-ts-mode))
(add-hook 'html-ts-mode-hook #'eglot-ensure)
(provide 'funmacs-html)
