;;; funmacs-toml.el --- TOML + Eglot + Tree-sitter setup  -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides TOML editing with tree-sitter and Eglot using the Taplo LSP.
;; Requires: Emacs 29+ (treesit), taplo available on PATH.

;;; Code:
(use-package toml-ts-mode
  :ensure nil
  :hook
  ((toml-ts-mode . apheleia-mode)
   (toml-ts-mode . eglot-ensure))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(toml-ts-mode . ("taplo" "lsp" "stdio")))))

(provide 'funmacs-toml)
;;; funmacs-toml.el ends here
