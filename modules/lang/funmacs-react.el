;;; funmacs-react.el -*- lexical-binding: t; -*-
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-hook 'tsx-ts-mode-hook #'eglot-ensure)
(provide 'funmacs-react)
