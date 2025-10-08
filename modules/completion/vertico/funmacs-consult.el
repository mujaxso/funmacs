;;; funmacs-consult.el --- Consult enhancements -*- lexical-binding: t; -*-
;;; Commentary:
;; Adds better search, buffer, and file navigation commands
;;; Code:

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ;; restore default open file behavior:
         ("C-x C-f" . find-file)
         ("M-y" . consult-yank-pop)))

(provide 'funmacs-consult)
;;; funmacs-consult.el ends here
