;;; funmacs-orderless.el --- Fuzzy matching for minibuffer -*- lexical-binding: t; -*-

;;; Commentary:
;; Enable orderless fuzzy matching globally (including Vertico)

;;; Code:

(use-package orderless
  :ensure t
  :demand t
  :custom
  ;; Enable orderless everywhere
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  ;; For file completion, partial-completion keeps `/` and `~` working
  (completion-category-overrides
   '((file (styles partial-completion)))))

(provide 'funmacs-orderless)
;;; funmacs-orderless.el ends here
