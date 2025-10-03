;;; funmacs-eat.el --- Terminal configuration using EAT -*- lexical-binding: t; -*-

(use-package eat
  :defer t
  :commands (eat eat-other-window eat-project)
  :init
  ;; Set default shell for eat
  (setq eat-shell "/bin/bash") ;; Change to zsh/fish if you prefer

  ;; Better scrolling
  (setq eat-term-maximum-scrollback 10000)

  ;; Keybindings
  (global-set-key (kbd "C-c t") #'eat)
  (global-set-key (kbd "C-c T") #'eat-other-window)

  :config
  ;; Integrate with project.el
  (defun funmacs-eat-project ()
    "Open an EAT terminal in the project root."
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (eat-other-window)))
  (global-set-key (kbd "C-c p t") #'funmacs-eat-project)

  ;; Optional: use nerd-icons for buffer names
  (with-eval-after-load 'nerd-icons
    (add-to-list 'eat-term-name-aliases '("term" . "")))
  )

(provide 'funmacs-eat)

;;; funmacs-eat.el ends here
