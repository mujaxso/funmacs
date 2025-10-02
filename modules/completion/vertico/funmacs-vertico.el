;;; funmacs-vertico.el --- Vertico completion module -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides vertical completion UI with Vertico.
;; Works well with Orderless and Corfu for a modern completion experience.

;;; Code:

(use-package vertico
  :ensure t
  :init
  (vertico-mode 1))

(use-package savehist
  :init
  (savehist-mode 1))

(provide 'funmacs-vertico)

;;; funmacs-vertico.el ends here
