;;; funmacs-docker.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Dockerfile settings using built-in dockerfile-ts-mode with Eglot.

;;; Code:

(use-package dockerfile-ts-mode
  :ensure nil
  :defer t
  :mode (("/Dockerfile\\'"   . dockerfile-ts-mode) 
         ("\\`Dockerfile\\'" . dockerfile-ts-mode) 
         ("\\.dockerfile\\'" . dockerfile-ts-mode) 
         ("\\.docker\\'"     . dockerfile-ts-mode)) 
  :init
  ;; Prefer the tree-sitter mode when legacy dockerfile-mode is present.
  (with-eval-after-load 'treesit
    (add-to-list 'major-mode-remap-alist '(dockerfile-mode . dockerfile-ts-mode))) 
  :hook
  (dockerfile-ts-mode . eglot-ensure))

(provide 'funmacs-docker)

;;; funmacs-docker.el ends here
