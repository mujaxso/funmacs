;;; funmacs-javascript.el -*- lexical-binding: t; -*-
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-hook 'js-ts-mode-hook #'eglot-ensure)
(provide 'funmacs-javascript)
