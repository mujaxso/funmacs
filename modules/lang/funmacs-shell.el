;;; funmacs-shell.el --- Shell script configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Shell script configuration with Tree-sitter and Eglot.

;;; Code:

(use-package bash-ts-mode
  :ensure nil
  :mode (("\\.sh\\'" . bash-ts-mode)
         ("\\.bash\\'" . bash-ts-mode)
         ("\\.zsh\\'" . bash-ts-mode)
         ("\\.command\\'" . bash-ts-mode))
  :hook (bash-ts-mode . eglot-ensure)
  :config
  (setq bash-ts-mode-indent-offset 2
        sh-basic-offset 2
        indent-tabs-mode nil))

(provide 'funmacs-shell)

;;; funmacs-shell.el ends here
