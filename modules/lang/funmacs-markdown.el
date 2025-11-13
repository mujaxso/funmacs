;;; funmacs-markdown.el --- Beautiful Markdown editing with preview -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern markdown editing with syntax highlighting, header scaling, and live preview.
;; Features:
;; - Beautiful syntax highlighting with scaled headers
;; - Auto-updating preview in right split window
;; - Native rendering with shr (basic HTML support)
;; - Table alignment with valign
;; - Prettified checkboxes

;;; Code:

;; Remove markdown-mode remapping to markdown-ts-mode
(setq major-mode-remap-alist
      (assoc-delete-all 'markdown-mode major-mode-remap-alist))

;; Markdown mode configuration
(use-package markdown-mode
  :ensure t
  :defer nil
  :demand t
  :init
  (setq markdown-fontify-code-blocks-natively t
        markdown-header-scaling t
        markdown-header-scaling-values '(1.8 1.5 1.3 1.1 1.0 1.0)
        markdown-asymmetric-header t
        markdown-hide-markup nil
        markdown-hide-urls nil
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-uppercase-checkbox t)
  :config
  ;; Clear any markdown-ts-mode associations
  (setq auto-mode-alist (delete '("\\.md\\'" . markdown-ts-mode) auto-mode-alist))
  (setq auto-mode-alist (delete '("\\.markdown\\'" . markdown-ts-mode) auto-mode-alist))
  (setq auto-mode-alist (delete '("README\\.md\\'" . markdown-ts-mode) auto-mode-alist))
  
  ;; Add markdown-mode associations
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . markdown-mode))
  
  ;; Keybindings
  (define-key markdown-mode-map (kbd "C-c C-c p") #'funmacs-markdown-toggle-preview)
  (define-key markdown-mode-map (kbd "C-c C-c h") #'markdown-toggle-markup-hiding)
  (define-key markdown-mode-map (kbd "C-c C-c i") #'markdown-toggle-inline-images))

;; Hooks
(add-hook 'markdown-mode-hook #'visual-line-mode)
(add-hook 'markdown-mode-hook #'funmacs-markdown-setup)

;; Setup function
(defun funmacs-markdown-setup ()
  "Setup markdown with beautiful highlighting."
  (setq-local line-spacing 0.2)
  (setq-local prettify-symbols-alist
              '(("[ ]" . "☐")
                ("[x]" . "☑")
                ("[X]" . "☑")
                ("[-]" . "◐")))
  (prettify-symbols-mode 1)
  (setq-local markdown-hide-markup nil))

;; Force preview window to open on the right side
(add-to-list 'display-buffer-alist
             '("\\*Markdown Preview\\*"
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.5)))

;; Configure shr renderer
(setq shr-use-colors t)
(setq shr-width nil)

;; Preview functionality
(defvar-local funmacs-markdown-preview-timer nil
  "Timer for auto-refreshing preview.")

(defun funmacs-markdown-preview-render ()
  "Render markdown preview using shr."
  (interactive)
  (let ((filename buffer-file-name)
        (preview-buffer "*Markdown Preview*")
        (scroll-pos (when-let ((win (get-buffer-window "*Markdown Preview*")))
                      (with-selected-window win (point)))))
    
    ;; Generate HTML with pandoc
    (shell-command-on-region
     (point-min)
     (point-max)
     "pandoc --from=gfm --to=html5 --standalone --quiet"
     preview-buffer)
    
    ;; Display in right window
    (let ((preview-window (display-buffer preview-buffer)))
      (with-selected-window preview-window
        (let ((document (libxml-parse-html-region (point-min) (point-max)))
              (url (concat "file://" (or filename default-directory))))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (shr-insert-document `(base ((href . ,url)) ,document))
            (when scroll-pos
              (goto-char (min scroll-pos (point-max))))
            (setq buffer-read-only t)))))))

(defun funmacs-markdown-auto-refresh-preview ()
  "Auto-refresh preview after idle time."
  (when (and (eq major-mode 'markdown-mode)
             (get-buffer "*Markdown Preview*"))
    (when funmacs-markdown-preview-timer
      (cancel-timer funmacs-markdown-preview-timer))
    (setq funmacs-markdown-preview-timer
          (run-with-idle-timer 0.5 nil #'funmacs-markdown-preview-render))))

(defun funmacs-markdown-toggle-preview ()
  "Toggle markdown preview in right split window."
  (interactive)
  (if (get-buffer "*Markdown Preview*")
      ;; Close preview
      (progn
        (when-let ((win (get-buffer-window "*Markdown Preview*")))
          (delete-window win))
        (kill-buffer "*Markdown Preview*")
        (remove-hook 'after-change-functions #'funmacs-markdown-auto-refresh-preview t)
        (message "Preview closed"))
    ;; Open preview
    (funmacs-markdown-preview-render)
    (add-hook 'after-change-functions
              (lambda (&rest _) (funmacs-markdown-auto-refresh-preview))
              nil t)
    (message "Preview opened - updates automatically")))

;; Table alignment with valign
(use-package valign
  :ensure t
  :hook (markdown-mode . valign-mode)
  :custom
  (valign-fancy-bar t))

;; Beautiful syntax highlighting faces
(custom-set-faces
 '(markdown-header-delimiter-face ((t (:foreground "#6e7681" :weight bold))))
 '(markdown-header-face-1 ((t (:height 1.8 :weight extra-bold :foreground "#79c0ff"))))
 '(markdown-header-face-2 ((t (:height 1.5 :weight extra-bold :foreground "#79c0ff"))))
 '(markdown-header-face-3 ((t (:height 1.3 :weight bold :foreground "#8ab4f8"))))
 '(markdown-header-face-4 ((t (:height 1.2 :weight bold :foreground "#8ab4f8"))))
 '(markdown-header-face-5 ((t (:height 1.1 :weight semi-bold :foreground "#9ecbff"))))
 '(markdown-header-face-6 ((t (:height 1.05 :weight semi-bold :foreground "#9ecbff"))))
 '(markdown-code-face ((t (:background "#161b22" :foreground "#f0883e"))))
 '(markdown-inline-code-face ((t (:background "#30363d" :foreground "#f0883e"))))
 '(markdown-pre-face ((t (:background "#161b22" :foreground "#c9d1d9"))))
 '(markdown-link-face ((t (:foreground "#58a6ff" :weight bold))))
 '(markdown-url-face ((t (:foreground "#a5d6ff" :underline t))))
 '(markdown-html-tag-face ((t (:foreground "#7ee787"))))
 '(markdown-html-attr-name-face ((t (:foreground "#79c0ff"))))
 '(markdown-html-attr-value-face ((t (:foreground "#a5d6ff"))))
 '(markdown-list-face ((t (:foreground "#79c0ff" :weight bold))))
 '(markdown-blockquote-face ((t (:foreground "#8b949e" :slant italic))))
 '(markdown-bold-face ((t (:weight extra-bold :foreground "#c9d1d9"))))
 '(markdown-italic-face ((t (:slant italic :foreground "#c9d1d9")))))

(provide 'funmacs-markdown)
;;; funmacs-markdown.el ends here
