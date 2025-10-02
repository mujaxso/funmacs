;;; funmacs-svelte.el -*- lexical-binding: t; -*-

;;; Commentary:
;; svelet-mode settings

;;; code

(use-package svelte-mode
  :ensure t
  :mode ("\\.svelte\\'" . svelte-mode))
(add-hook 'svelte-mode-hook #'eglot-ensure)

(provide 'funmacs-svelte)

;;; funmacs-svelte.el ends here
