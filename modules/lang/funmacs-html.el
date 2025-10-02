;;; funmacs-html.el -*- lexical-binding: t; -*-

;;; Commentary:
;; html-mode settings

;;; code

(add-to-list 'auto-mode-alist '("\\.html?\\'" . html-ts-mode))
(add-hook 'html-ts-mode-hook #'eglot-ensure)

(provide 'funmacs-html)

;;; funmacs-html.el ends here
