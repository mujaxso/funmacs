;;; funmacs-modeline.el -*- lexical-binding: t; -*-

;;; Commentary:
;; modeline settings

;;; code

(use-package doom-modeline
  :ensure t
  :demand t
  :init
  (setq doom-modeline-height 25
        doom-modeline-bar-width 3
        doom-modeline-window-width-limit 85
        doom-modeline-minor-modes t
        doom-modeline-icon nil
        doom-modeline-major-mode-icon nil
        doom-modeline-major-mode-color-icon nil
        doom-modeline-buffer-state-icon nil
        doom-modeline-buffer-modification-icon nil
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
  (doom-modeline-mode 1))

(provide 'funmacs-modeline)

;;; funmacs-modeline.el ends here
