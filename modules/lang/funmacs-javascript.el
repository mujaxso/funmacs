;;; funmacs-javascript.el -*- lexical-binding: t; -*-

;;; Commentary:
;; javascript-mode settings

;;; code

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-hook 'js-ts-mode-hook #'eglot-ensure)

(provide 'funmacs-javascript)

;;; funmacs-javascript.el ends here
