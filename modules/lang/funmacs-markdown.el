;;; funmacs-markdown.el --- Modern Markdown with same-buffer preview -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern markdown editing with same-buffer preview, clean display, and context-aware symbol visibility.

;;; Code:

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)
         ("README\\.md\\'" . gfm-mode))
  :custom
  ;; Core markdown settings
  (markdown-command "pandoc -f gfm -t html5")
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  (markdown-asymmetric-header t)
  ;; Enable markup hiding by default for clean display
  (markdown-hide-markup t)
  (markdown-hide-urls t)
  :config
  ;; Enable markup hiding by default in all markdown buffers
  (add-hook 'markdown-mode-hook #'markdown-toggle-markup-hiding)
  
  ;; Context-aware symbol visibility - show markup on current line only
  (defvar-local funmacs--markup-hidden-p nil
    "Track if markup is currently hidden.")
  
  (defun funmacs/markdown-reveal-current-line ()
    "Temporarily reveal markdown markup on the current line."
    (when (and markdown-hide-markup (derived-mode-p 'markdown-mode))
      (let ((inhibit-point-motion-hooks t)
            (inhibit-modification-hooks t))
        (remove-text-properties (line-beginning-position) 
                               (line-end-position)
                               '(invisible markdown-markup)))))
  
  (defun funmacs/markdown-hide-markup-except-current-line ()
    "Hide markup except on the current line."
    (when (and markdown-hide-markup (derived-mode-p 'markdown-mode))
      (save-excursion
        (save-restriction
          (widen)
          (let ((inhibit-point-motion-hooks t)
                (inhibit-modification-hooks t)
                (current-line (line-number-at-pos)))
            (markdown-map-region 
             (lambda ()
               (when (/= (line-number-at-pos) current-line)
                 (markdown-put-text-property (point) (markdown-line-end-position)
                                            'invisible 'markdown-markup)))
             (point-min) (point-max)))))))
  
  ;; Same-buffer preview using shr (eww rendering engine)
  (defun funmacs/markdown-preview-same-buffer ()
    "Render markdown as HTML in the same buffer using shr."
    (interactive)
    (unless (executable-find "pandoc")
      (user-error "pandoc is required for markdown preview"))
    
    (let ((original-buffer (current-buffer))
          (original-point (point))
          (html-buffer (get-buffer-create "*markdown-preview*")))
      ;; Convert markdown to HTML
      (with-current-buffer html-buffer
        (erase-buffer)
        (insert (shell-command-to-string 
                 (format "echo '%s' | pandoc -f gfm -t html5"
                         (shell-quote-argument 
                          (buffer-substring-no-properties (point-min) (point-max))))))
        ;; Render HTML with shr
        (shr-render-region (point-min) (point-max)))
      
      ;; Replace current buffer with preview
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-buffer-substring html-buffer)
        (goto-char original-point)
        (view-mode 1)
        (message "Markdown preview - press 'q' to return to editing"))
      
      ;; Set up key to return to editing
      (local-set-key (kbd "q") 
                     (lambda () 
                       (interactive)
                       (view-mode -1)
                       (erase-buffer)
                       (insert-buffer-substring original-buffer)
                       (goto-char original-point)))))
  
  ;; Toggle between markup hidden and visible
  (defun funmacs/markdown-toggle-markup-context ()
    "Toggle between hiding markup and showing it only on current line."
    (interactive)
    (if markdown-hide-markup
        (progn
          (markdown-toggle-markup-hiding -1)
          (remove-hook 'post-command-hook #'funmacs/markdown-reveal-current-line t)
          (remove-hook 'post-command-hook #'funmacs/markdown-hide-markup-except-current-line t)
          (message "Markdown markup visible"))
      (progn
        (markdown-toggle-markup-hiding 1)
        (add-hook 'post-command-hook #'funmacs/markdown-reveal-current-line nil t)
        (add-hook 'post-command-hook #'funmacs/markdown-hide-markup-except-current-line nil t)
        (message "Markdown markup hidden (context-aware)"))))
  
  ;; Keybindings
  (define-key markdown-mode-map (kbd "C-c C-p") #'funmacs/markdown-preview-same-buffer)
  (define-key markdown-mode-map (kbd "C-c C-h") #'funmacs/markdown-toggle-markup-context))

;; Setup function for modern markdown editing
(defun funmacs/markdown-setup ()
  "Setup markdown with modern features and clean appearance."
  (setq-local line-spacing 0.1)
  (visual-line-mode 1)  ; Soft wrapping for document-like feel
  
  ;; Enable context-aware markup visibility
  (when markdown-hide-markup
    (add-hook 'post-command-hook #'funmacs/markdown-reveal-current-line nil t)
    (add-hook 'post-command-hook #'funmacs/markdown-hide-markup-except-current-line nil t))
  
  ;; Modern appearance settings
  (setq-local word-wrap t)
  (setq-local fill-column 80))

(add-hook 'markdown-mode-hook #'funmacs/markdown-setup)
(add-hook 'gfm-mode-hook #'funmacs/markdown-setup)

;; Clean, modern heading faces for rendered appearance
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
