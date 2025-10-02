;;; funmacs-rust.el -*- lexical-binding: t; -*-

;;; Commentary:
;; rust-mode settings

;;; code

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-hook 'rust-ts-mode-hook #'eglot-ensure)

(provide 'funmacs-rust)

;;; funmacs-rust.el ends here
