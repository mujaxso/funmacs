;;; funmacs-markdown.el --- Enhanced Markdown with inline rendering -*- lexical-binding: t; -*-

;;; Commentary:
;; Markdown editing with org-mode-style inline rendering and GitHub-style HTML preview in Emacs buffer.

;;; Code:

;; Markdown mode with inline rendering features
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode)
         ("README\\.md\\'" . gfm-mode))
  :init
  (setq markdown-command "pandoc -f gfm -t html5 --mathjax --highlight-style=github")
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . funmacs-markdown-setup)
         (gfm-mode . visual-line-mode)
         (gfm-mode . funmacs-markdown-setup))
  :custom
  ;; Core settings for inline rendering
  (markdown-fontify-code-blocks-natively t)
  (markdown-header-scaling t)
  (markdown-header-scaling-values '(1.8 1.5 1.3 1.1 1.0 1.0))
  (markdown-asymmetric-header t)
  (markdown-make-gfm-checkboxes-buttons t)
  (markdown-gfm-uppercase-checkbox t)
  ;; Built-in live preview settings
  (markdown-split-window-direction 'right)  ; Preview on right side
  :config
  ;; Keybindings
  (define-key markdown-mode-map (kbd "C-c C-c l") #'markdown-live-preview-mode)
  (define-key markdown-mode-map (kbd "C-c C-c p") #'funmacs-markdown-preview-buffer)
  (define-key markdown-mode-map (kbd "C-c C-c r") #'funmacs-markdown-preview-refresh))

;; Setup function for markdown mode
(defun funmacs-markdown-setup ()
  "Setup markdown with inline rendering."
  (setq-local line-spacing 0.2)
  
  ;; Enable markup hiding after buffer loads
  (run-with-idle-timer 0.1 nil
                       (lambda (buf)
                         (when (buffer-live-p buf)
                           (with-current-buffer buf
                             (when (or (eq major-mode 'markdown-mode)
                                       (eq major-mode 'gfm-mode))
                               (markdown-toggle-markup-hiding 1)))))
                       (current-buffer))
  
  ;; Display inline images if in graphical environment
  (when (display-graphic-p)
    (run-with-idle-timer 0.2 nil
                         (lambda (buf)
                           (when (buffer-live-p buf)
                             (with-current-buffer buf
                               (when (or (eq major-mode 'markdown-mode)
                                         (eq major-mode 'gfm-mode))
                                 (markdown-toggle-inline-images)))))
                         (current-buffer))))

;; GitHub-style preview in Emacs buffer using shr
(defun funmacs-markdown-preview-buffer ()
  "Preview markdown in Emacs buffer with GitHub styling."
  (interactive)
  (let ((filename buffer-file-name)
        (preview-buffer "*Markdown Preview*"))
    (message "Rendering GitHub-style Markdown preview...")
    ;; Generate HTML with GitHub styling
    (shell-command-on-region 
     (point-min) 
     (point-max)
     (concat "pandoc -f gfm -t html5 --standalone --mathjax "
             "--highlight-style=github "
             "--css=https://cdn.jsdelivr.net/npm/github-markdown-css@5.5.0/github-markdown-dark.min.css "
             "--metadata title=\"Markdown Preview\"")
     preview-buffer)
    ;; Render in other window
    (save-selected-window
      (switch-to-buffer-other-window preview-buffer)
      (let ((document (libxml-parse-html-region (point-min) (point-max)))
            (url (concat "file://" (or filename default-directory))))
        (erase-buffer)
        (shr-insert-document `(base ((href . ,url)) ,document))
        (goto-char (point-min))
        (read-only-mode 1)))
    (message "Preview rendered in %s" preview-buffer)))

;; Auto-refresh preview
(defun funmacs-markdown-preview-refresh ()
  "Refresh the markdown preview buffer if it exists."
  (interactive)
  (when (get-buffer "*Markdown Preview*")
    (funmacs-markdown-preview-buffer)))

;; Auto-refresh on save
(defun funmacs-markdown-auto-refresh-preview ()
  "Auto-refresh preview on save."
  (when (and (or (eq major-mode 'markdown-mode)
                 (eq major-mode 'gfm-mode))
             (get-buffer "*Markdown Preview*"))
    (funmacs-markdown-preview-buffer)))

(add-hook 'after-save-hook #'funmacs-markdown-auto-refresh-preview)

;; Reveal markup on current line when editing
(defvar funmacs-markdown-current-line '(0 . 0)
  "(start . end) of current line in current buffer.")
(make-variable-buffer-local 'funmacs-markdown-current-line)

(defun funmacs-markdown-unhide-current-line (limit)
  "Font-lock function to reveal markup on current line."
  (let ((start (max (point) (car funmacs-markdown-current-line)))
        (end (min limit (cdr funmacs-markdown-current-line))))
    (when (< start end)
      (remove-text-properties start end
                              '(invisible t display "" composition ""))
      (goto-char limit)
      t)))

(defun funmacs-markdown-refontify-on-linemove ()
  "Post-command-hook to refontify when moving lines."
  (let* ((start (line-beginning-position))
         (end (line-beginning-position 2))
         (needs-update (not (equal start (car funmacs-markdown-current-line)))))
    (setq funmacs-markdown-current-line (cons start end))
    (when needs-update
      (font-lock-fontify-block 3))))

(defun funmacs-markdown-enable-smart-hiding ()
  "Enable markup hiding with reveal-on-edit behavior."
  (interactive)
  (markdown-toggle-markup-hiding 1)
  (font-lock-add-keywords nil '((funmacs-markdown-unhide-current-line)) t)
  (add-hook 'post-command-hook #'funmacs-markdown-refontify-on-linemove nil t)
  (font-lock-flush))

;; Add to markdown hook
(add-hook 'markdown-mode-hook #'funmacs-markdown-enable-smart-hiding)
(add-hook 'gfm-mode-hook #'funmacs-markdown-enable-smart-hiding)

;; Valign for table alignment
(use-package valign
  :ensure t
  :hook ((markdown-mode . valign-mode)
         (gfm-mode . valign-mode))
  :custom
  (valign-fancy-bar t))

;; Prettier heading faces
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
