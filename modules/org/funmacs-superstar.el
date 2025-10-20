;;; funmacs-superstar.el --- Complete Org Superstar Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Full-featured org-superstar setup with all customization options

;;; Code:

(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  
  ;; ==========================================================================
  ;; HEADLINE BULLETS
  ;; ==========================================================================
  
  ;; Set custom UTF-8 bullets for different heading levels
  (setq org-superstar-headline-bullets-list
        '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶"))
  
  ;; Cycle through bullets list for deeper levels
  (setq org-superstar-cycle-headline-bullets t)
  
  ;; ==========================================================================
  ;; LEADING STARS
  ;; ==========================================================================
  
  ;; Prettify leading stars
  (setq org-superstar-prettify-leading-stars t)
  
  ;; Character to display for leading stars
  (setq org-superstar-leading-bullet ?\s)  ; space character for clean look
  ;; Alternative: (setq org-superstar-leading-bullet ?·)  ; middle dot
  
  ;; Fallback character for terminal sessions
  (setq org-superstar-leading-fallback ?\s)
  
  ;; Remove leading stars entirely (no indentation)
  (setq org-superstar-remove-leading-stars nil)
  ;; Set to t if you want no indentation from leading stars
  
  ;; ==========================================================================
  ;; LIST BULLETS
  ;; ==========================================================================
  
  ;; Enable prettifying of plain list bullets
  (setq org-superstar-prettify-item-bullets t)
  
  ;; Custom bullets for list items (*, +, -)
  (setq org-superstar-item-bullet-alist
        '((?* . ?•)   ; asterisk -> bullet
          (?+ . ?➤)   ; plus -> arrow
          (?- . ?–))) ; hyphen -> en-dash
  
  ;; Alternative fancy bullets:
  ;; '((?* . ?●)
  ;;   (?+ . ?◆)
  ;;   (?- . ?▸))
  
  ;; ==========================================================================
  ;; TODO ITEM BULLETS
  ;; ==========================================================================
  
  ;; Enable special bullets for TODO items
  (setq org-superstar-special-todo-items t)
  ;; Set to 'hide to hide bullets for TODO items entirely
  ;; Set to nil to disable special TODO bullets
  
  ;; Custom bullets for TODO keywords
  (setq org-superstar-todo-bullet-alist
        '(("TODO" . ?☐)
          ("IN-PROGRESS" . ?⚙)
          ("WAITING" . ?⏳)
          ("DONE" . ?✓)
          ("CANCELLED" . ?✗)))
  
  ;; Alternative TODO bullets:
  ;; '(("TODO" . ?⬜)
  ;;   ("IN-PROGRESS" . ?▶)
  ;;   ("WAITING" . ?⏸)
  ;;   ("DONE" . ?✔)
  ;;   ("CANCELLED" . ?❌))
  
  ;; ==========================================================================
  ;; INLINE TASKS
  ;; ==========================================================================
  
  ;; First inline task bullet (when org-inlinetask-show-first-star is non-nil)
  (setq org-superstar-first-inlinetask-bullet ?★)
  
  ;; Fallback for terminal sessions
  (setq org-superstar-first-inlinetask-fallback ?*)
  
  ;; ==========================================================================
  ;; CUSTOM FACES
  ;; ==========================================================================
  
  ;; Face for header bullets (difference from org-level-N)
  (custom-set-faces
   '(org-superstar-header-bullet
     ((t (:inherit default :foreground "#ff6c6b" :weight bold))))
   
   ;; Face for leading stars
   '(org-superstar-leading
     ((t (:inherit default :foreground "#3a3f4b"))))
   
   ;; Face for list item bullets
   '(org-superstar-item
     ((t (:inherit default :foreground "#51afef"))))
   
   ;; Face for first inline task marker
   '(org-superstar-first
     ((t (:inherit org-warning :foreground "#ECBE7B")))))
  
  ;; ==========================================================================
  ;; PERFORMANCE OPTIONS
  ;; ==========================================================================
  
  ;; Toggle lightweight list mode for large files
  ;; This disables syntax checking for plain lists (faster but less accurate)
  ;; (org-superstar-toggle-lightweight-lists)
  
  ;; Automatically enable lightweight mode for files with many list items
  (defun my-org-superstar-auto-lightweight ()
    "Enable lightweight list mode if file has many list items."
    (let ((list-items
           (count-matches "^[ \t]*?\\([+-]\\|[ \t]\\*\\)"
                          (point-min) (point-max))))
      (when (> list-items 100)
        (org-superstar-toggle-lightweight-lists))))
  
  (add-hook 'org-mode-hook #'my-org-superstar-auto-lightweight)
  
  ;; ==========================================================================
  ;; COMPATIBILITY FUNCTION
  ;; ==========================================================================
  
  ;; Make it behave like org-bullets (if desired)
  ;; Uncomment to use:
  ;; (org-superstar-configure-like-org-bullets)
  
  )

;; =============================================================================
;; ADDITIONAL ORG VISUAL ENHANCEMENTS
;; =============================================================================

;; Hide emphasis markers (*, /, _, etc.)
(setq org-hide-emphasis-markers t)

;; Enable pretty entities (display \alpha as α, etc.)
(setq org-pretty-entities t)

;; Hide leading stars (works with org-superstar)
(setq org-hide-leading-stars t)

;; Font settings for Org headings (optional)
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.3 :weight bold))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.2 :weight bold))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1 :weight semi-bold))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.05 :weight semi-bold))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(org-document-title ((t (:height 1.5 :weight bold :underline nil)))))

(provide 'funmacs-superstar)
;;; funmacs-superstar.el ends here
