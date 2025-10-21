;;; funmacs-vundo.el -*- lexical-binding: t; -*-

;;; Commentary:
;; undo/redo package

;;; code

(use-package vundo
  :bind (("C-x u" . vundo))
  :config
  (setq vundo-compact-display t)
  ;; Increase undo limits to prevent truncation
  (setq undo-limit 800000)           ;; default: ~160k
  (setq undo-strong-limit 12000000)  ;; default: ~240k
  (setq undo-outer-limit 120000000)  ;; default: ~24MB
  )

(provide 'funmacs-vundo)

;;; funmacs-vundo.el ends here
