;;; funmacs-vue.el -*- lexical-binding: t; -*-

;;; Commentary:
;; vue-mode settings

;;; code

(use-package vue-mode
  :ensure t
  :mode ("\\.vue\\'" . vue-mode))
(add-hook 'vue-mode-hook #'eglot-ensure)

(provide 'funmacs-vue)

;;; funmacs-vue.el ends here
