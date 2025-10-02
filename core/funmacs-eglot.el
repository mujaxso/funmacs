;;; funmacs-eglot.el -*- lexical-binding: t; -*-

;;; Commentary:
;; built in lsp client

;;;code

(use-package eglot
  :defer t
  :config
  (unless (or (executable-find "clangd") (executable-find "ccls"))
    (message "[Funmacs] Warning: clangd/ccls not found - C/C++ LSP will not start.")))

(provide 'funmacs-eglot)

;;; funmacs-eglot.el ends here
