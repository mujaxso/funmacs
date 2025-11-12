;;; funmacs-modeline.el -*- lexical-binding: t; -*-

;;; Commentary:
;; modeline settings

;;; code

(use-package doom-modeline
  :ensure t
  :after nerd-icons
  :init
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-window-width-limit 85
        doom-modeline-minor-modes t
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-state-icon t
        doom-modeline-buffer-modification-icon t
        doom-modeline-unicode-fallback t
        doom-modeline-buffer-name t
        doom-modeline-project-detection 'project
        doom-modeline-persp-name t
        doom-modeline-display-default-persp-name nil
        doom-modeline-lsp t
        doom-modeline-github nil
        doom-modeline-gnus t
        doom-modeline-mu4e t
        doom-modeline-irc t
        doom-modeline-checker-simple-format t
        doom-modeline-vcs-max-length 12
        doom-modeline-workspace-name t
        doom-modeline-persp-name t)
  :config
  (doom-modeline-mode 1)
  
  ;; Fallback to ensure modeline is visible
  (unless mode-line-format
    (setq mode-line-format 
          '("%e" 
            mode-line-front-space
            mode-line-mule-info
            mode-line-client
            mode-line-modified
            mode-line-remote
            mode-line-frame-identification
            mode-line-buffer-identification
            "   "
            mode-line-position
            (vc-mode vc-mode)
            "  "
            mode-line-modes
            mode-line-misc-info
            mode-line-end-spaces))))

(provide 'funmacs-modeline)

;;; funmacs-modeline.el ends here
