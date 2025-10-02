;;; funmacs-json.el -*- lexical-binding: t; -*-

;;; Commentary:
;; json-mode settings

;;; code

(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-hook 'json-ts-mode-hook #'eglot-ensure)

(provide 'funmacs-json)

;;; funamcs-json.el ends here
