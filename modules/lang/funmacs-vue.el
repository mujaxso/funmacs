;;; funmacs-vue.el -*- lexical-binding: t; -*-
(use-package vue-mode :ensure t :mode ("\\.vue\\'" . vue-mode))
(add-hook 'vue-mode-hook #'eglot-ensure)
(provide 'funmacs-vue)
