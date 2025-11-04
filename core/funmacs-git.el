;;; funmacs-git.el --- magit -*- lexical-binding: t; -*-

;;; Commentary:
;; magit as git ui.

;;; Code:

;; Ensure VC Git backend is loaded
(require 'vc-git)

;; Make sure Git is in handled backends
(add-to-list 'vc-handled-backends 'Git)

(use-package magit
  :ensure t
  :defer t
  :commands (magit-status magit-log)
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch))
  :custom
  (git-commit-summary-max-length 50)
  (git-commit-fill-column 72)
  :config
  ;; Refresh magit status after saving buffers
  (add-hook 'after-save-hook #'magit-after-save-refresh-status t))

(use-package forge
  :ensure t
  :after magit
  )

(use-package magit-todos
  :ensure t
  :after magit
  :hook (magit-mode . magit-todos-mode)
  :custom
  (magit-todos-keywords '("TODO" "FIXME" "HACK" "NOTE"))
  )

(use-package magit-org-todos
  :ensure t
  :after magit
  :config
  (magit-org-todos-autoinsert))

(use-package magit-prime
  :ensure t
  :after magit
  :config
  (magit-prime-mode 1))

(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode +1)
  :custom
  (git-gutter:update-interval 2))


(provide 'funmacs-git)

;;; funmacs-git.el ends here
