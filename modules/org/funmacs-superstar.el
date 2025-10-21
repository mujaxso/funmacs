;;; funmacs-superstar.el --- Complete Org Superstar Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Beautiful org-superstar setup with refined Unicode, performance tweaks, and custom faces.

;;; Code:

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :config
  ;; ==========================
  ;; Headline Bullets
  ;; ==========================
  (setq org-superstar-headline-bullets-list
        '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶"))
  (setq org-superstar-cycle-headline-bullets t)
  
  ;; ==========================
  ;; Leading Stars
  ;; ==========================
  (setq org-superstar-prettify-leading-stars t)
  (setq org-superstar-leading-bullet ?\s)
  (setq org-superstar-leading-fallback ?\s)
  (setq org-superstar-remove-leading-stars nil)

  ;; ==========================
  ;; List Bullets
  ;; ==========================
  (setq org-superstar-prettify-item-bullets t)
  (setq org-superstar-item-bullet-alist
        '((?* . ?•)
          (?+ . ?➤)
          (?- . ?–)))

  ;; ==========================
  ;; TODO Item Bullets
  ;; ==========================
  (setq org-superstar-special-todo-items t)
  (setq org-superstar-todo-bullet-alist
        '(("TODO" . ?☐)
          ("NEXT" . ?✒)
          ("IN-PROGRESS" . ?⚙)
          ("WAITING" . ?⏳)
          ("HOLD" . ?✰)
          ("DONE" . ?✓)
          ("CANCELLED" . ?✗)))
  
  ;; ==========================
  ;; Inline Tasks
  ;; ==========================
  (setq org-superstar-first-inlinetask-bullet ?★)
  (setq org-superstar-first-inlinetask-fallback ?*)

  ;; ==========================
  ;; Faces
  ;; ==========================
  (with-eval-after-load 'org-superstar
    (custom-set-faces
     '(org-superstar-header-bullet ((t (:inherit default :foreground "#ff6c6b" :weight bold))))
     '(org-superstar-leading       ((t (:inherit default :foreground "#3a3f4b"))))
     '(org-superstar-item          ((t (:inherit default :foreground "#51afef"))))
     '(org-superstar-first         ((t (:inherit org-warning :foreground "#ECBE7B"))))))
  
  ;; ==========================
  ;; Performance
  ;; ==========================
  ;; Auto-toggle lightweight list mode for large files (100+ list items)
  (defun funmacs/org-superstar-auto-lightweight ()
    "Enable lightweight list mode for large Org files."
    (when (and (boundp 'org-superstar-mode)
               org-superstar-mode)
      (let ((count (count-matches "^[ \t]*?\\([+-]\\|[ \t]\\*\\)" (point-min) (point-max))))
        (when (> count 100)
          (org-superstar-toggle-lightweight-lists)))))
  (add-hook 'org-mode-hook #'funmacs/org-superstar-auto-lightweight)
  )

;; ==========================
;; Additional Visual Enhancements
;; ==========================

(setq org-hide-emphasis-markers t)
(setq org-pretty-entities t)
(setq org-hide-leading-stars t)

(with-eval-after-load 'org
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.3 :weight bold))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.2 :weight bold))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.1 :weight semi-bold))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.05 :weight semi-bold))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
   '(org-document-title ((t (:height 1.5 :weight bold :underline nil))))))

;; Optionally set org-ellipsis for folded outlines:
(setq org-ellipsis " ▼ ")

(provide 'funmacs-superstar)
;;; funmacs-superstar.el ends here
