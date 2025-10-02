;;; funmacs-markdown.el --- Markdown support -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides Markdown editing and preview inside Emacs.

;;; Code:

;; Markdown editing mode
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "pandoc"))

;; Live preview in a separate buffer
(use-package markdown-preview-mode
  :ensure t
  :commands (markdown-preview-mode)
  :bind (:map markdown-mode-command-map
              ("p" . markdown-preview-mode)))

(provide 'funmacs-markdown)

;;; funmacs-markdown.el ends here
