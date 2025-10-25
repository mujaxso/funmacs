;;; funmacs-vundo.el -*- lexical-binding: t; -*-

;;; Commentary:
;; undo/redo package

;;; code

(use-package vundo
  :bind
  (("C-x u" . funmacs/vundo-open)
   (:map meow-normal-state-keymap
         ("u" . funmacs/vundo-open)))
  :config
  ;; Appearance
  (setq vundo-glyph-alist vundo-unicode-symbols
        vundo-compact-display t)

  ;; Undo limits
  (setq undo-limit 800000)           ;; default: ~160k
  (setq undo-strong-limit 12000000)  ;; default: ~12MB
  (setq undo-outer-limit 120000000)  ;; default: ~120MB

  ;; --- Fixed: prevent dedicated window error ---
  (defun funmacs/vundo-open ()
    "Open vundo in a non-dedicated window."
    (interactive)
    (let ((display-buffer-alist
           '(("\\*vundo-tree\\*"
              ;; Open in a normal, non-dedicated side window
              (display-buffer-in-side-window)
              (side . right)
              (window-width . 0.3)
              (dedicated . nil)))))
      (vundo))))

(provide 'funmacs-vundo)
;;; funmacs-vundo.el ends here
