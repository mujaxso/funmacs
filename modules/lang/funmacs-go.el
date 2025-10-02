;;; funmacs-go.el -*- lexical-binding: t; -*-

;;; Commentary:
;; go-mode settings

;;; code

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-hook 'go-ts-mode-hook #'eglot-ensure)

(provide 'funmacs-go)

;;; funmacs-go.el ends here
