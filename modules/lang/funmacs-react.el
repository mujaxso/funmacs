;;; funmacs-react.el -*- lexical-binding: t; -*-

;;; Commentary:
;; react-mode settings

;;; code

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-hook 'tsx-ts-mode-hook #'eglot-ensure)

(provide 'funmacs-react)

;;; funmacs-react.el ends here
