;;; funmacs-vundo.el -*- lexical-binding: t; -*-

;;; Commentary:
;; undo/redo package

;;; code

(use-package vundo
  :bind (("C-x u" . vundo))
  :config (setq vundo-compact-display t))

(provide 'funmacs-vundo)

;;; funmacs-vundo.el ends here
