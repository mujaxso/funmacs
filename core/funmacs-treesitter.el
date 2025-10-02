;;; funmacs-treesitter.el -*- lexical-binding: t; -*-

;;; Commentary:
;; tree-sitter settings

;;; code

(setq treesit-language-source-alist
      '((c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
        (nix . ("https://github.com/nix-community/tree-sitter-nix"))))

(defun funmacs--compiler-available-p () (or (executable-find "gcc") (executable-find "clang") (executable-find "cc")))
(defun funmacs-install-missing-grammars ()
  (interactive)
  (if (funmacs--compiler-available-p)
      (dolist (lang (mapcar #'car treesit-language-source-alist))
        (unless (treesit-language-available-p lang)
          (ignore-errors (message "[Funmacs] Installing tree-sitter grammar for %s..." lang) (treesit-install-language-grammar lang))))
    (message "[Funmacs] No C compiler found; skipping tree-sitter installs.")))
;; attempt install at startup but safe
(ignore-errors (funmacs-install-missing-grammars))
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (js-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (css-mode . css-ts-mode)
        (json-mode . json-ts-mode)
        (html-mode . html-ts-mode)
        (go-mode . go-ts-mode)
        (rust-mode . rust-ts-mode)))

(provide 'funmacs-treesitter)

;;; funmacs-treesitter.el ends here
