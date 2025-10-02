;;; funmacs-svelte.el -*- lexical-binding: t; -*-
(use-package svelte-mode :ensure t :mode ("\\.svelte\\'" . svelte-mode))
(add-hook 'svelte-mode-hook #'eglot-ensure)
(provide 'funmacs-svelte)
