;;; funmacs-typescript.el -*- lexical-binding: t; -*-

;;; Commentary:
;; typescript-mode settings

;;; code

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-hook 'typescript-ts-mode-hook #'eglot-ensure)
(add-hook 'tsx-ts-mode-hook #'eglot-ensure)

(provide 'funmacs-typescript)

;;; funmacs-typescript.el ends here
