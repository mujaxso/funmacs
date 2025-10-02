;;; funmacs-format.el -*- lexical-binding: t; -*-

;;; Commentary:
;; apheleia formatter

;;; Code:

(use-package apheleia
  :ensure t
  :defer t
  :hook
  ((prog-mode . apheleia-mode)   ;; enable in all programming modes
   (text-mode . apheleia-mode)) ;; optional, formats Markdown, etc.
  :config
  ;; Example formatters setup
  (setf (alist-get 'clang-format apheleia-formatters)
        '("clang-format" "-style=Google"))
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))

  ;; Language → formatter mapping
  (setf (alist-get 'c-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'js-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'typescript-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'json-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'css-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'html-mode apheleia-mode-alist) 'prettier))


(provide 'funmacs-format)

;;; funmacs-format.el ends here
