;;; funmacs-orderless.el --- Orderless completion module -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides advanced completion style using Orderless.
;; This replaces the default completion style with more flexible matching.

;;; Code:

(use-package orderless
  :ensure t
  :init
  ;; Configure completion styles globally
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles basic partial-completion)))))

(provide 'funmacs-orderless)

;;; funmacs-orderless.el ends here
