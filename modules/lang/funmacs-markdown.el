;;; funmacs-markdown.el --- Enhanced Markdown with inline rendering -*- lexical-binding: t; -*-

;;; Commentary:
;; Markdown editing with org-mode-style inline rendering and GitHub-style HTML preview in Emacs buffer.

;;; Code:

;; Clean, modern markdown editing
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)
         ("README\\.md\\'" . gfm-mode))
  :hook ((markdown-mode . funmacs-markdown-setup)
         (gfm-mode . funmacs-markdown-setup))
  :custom
  ;; Cleaner display settings
  (markdown-hide-markup t)  ; Hide formatting symbols for cleaner look
  (markdown-hide-urls t)    ; Hide URLs behind link text
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  (markdown-header-scaling-values '(1.8 1.5 1.3 1.1 1.0 1.0))
  (markdown-asymmetric-header t)
  ;; Built-in live preview settings
  (markdown-split-window-direction 'right)
  :config
  ;; Keybindings for preview
  (define-key markdown-mode-map (kbd "C-c C-c l") #'markdown-live-preview-mode)
  (define-key markdown-mode-map (kbd "C-c C-c p") #'markdown-preview)
  (define-key markdown-mode-map (kbd "C-c C-c e") #'markdown-export-and-preview))

;; Setup function for clean markdown editing
(defun funmacs-markdown-setup ()
  "Setup markdown with clean, modern display."
  (setq-local line-spacing 0.1)
  (visual-line-mode 1)  ; Soft wrapping
  (valign-mode 1)       ; Clean table alignment
  
  ;; Enable clean display features
  (markdown-toggle-markup-hiding 1)
  (markdown-toggle-url-hiding 1))

;; Valign for clean table alignment
(use-package valign
  :ensure t
  :hook ((markdown-mode . valign-mode)
         (gfm-mode . valign-mode))
  :custom
  (valign-fancy-bar t))

;; Clean, modern heading faces
(custom-set-faces
 '(markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9))))
 '(markdown-header-face-1 ((t (:height 1.8 :weight extra-bold :foreground "#79c0ff"))))
 '(markdown-header-face-2 ((t (:height 1.4 :weight extra-bold :foreground "#79c0ff"))))
 '(markdown-header-face-3 ((t (:height 1.2 :weight extra-bold :foreground "#79c0ff"))))
 '(markdown-header-face-4 ((t (:height 1.15 :weight bold :foreground "#79c0ff"))))
 '(markdown-header-face-5 ((t (:height 1.1 :weight bold :foreground "#79c0ff"))))
 '(markdown-header-face-6 ((t (:height 1.05 :weight semi-bold :foreground "#79c0ff")))))

(provide 'funmacs-markdown)
;;; funmacs-markdown.el ends here
