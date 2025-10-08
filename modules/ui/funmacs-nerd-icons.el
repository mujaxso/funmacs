;;; funmacs-nerd-icons.el --- Nerd Icons support -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides Nerd Font icons integration for completions and UI.
;; Requires that a Nerd Font (like FiraCode Nerd Font) is installed.

;;; Code:

(use-package nerd-icons
  :ensure t)

;; Icons in completion UI (with Marginalia or Corfu)
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :init
  (nerd-icons-completion-mode))

;; Icons in dired
(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

;; corfu
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'funmacs-nerd-icons)

;;; funmacs-nerd-icons.el ends here
