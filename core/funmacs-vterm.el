;;; funmacs-vterm.el --- VTerm configuration -*- lexical-binding: t; -*-

;;; Commentary
;; vterm terminal configuration

;;; code

(use-package vterm
  :ensure t
  :commands (vterm vterm-other-window)
  :after (vterm-toggle)
  :init
  (setq vterm-max-scrollback 10000)
  (setq vterm-buffer-name-string "vterm: %s")
  (setq vterm-toggle-fullscreen-p nil)
  
  ;; Keybindings
  ;; (global-set-key (kbd "C-c t") #'vterm)
  ;; (global-set-key (kbd "C-c T") #'vterm-other-window)

  :config
  ;; Project integration
  (defun funmacs-vterm-project ()
    "Open a vterm in the project root."
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (vterm-other-window)))
  (global-set-key (kbd "C-c p v") #'funmacs-vterm-project))

;; Enable vterm-toggle.el
(use-package vterm-toggle
  :bind
  (("C-c t"        . vterm-toggle)
   :map vterm-mode-map
   ("<C-return>" . vterm-toggle-insert-cd)
   ("s-n" . vterm-toggle-forward)
   ("s-p" . vterm-toggle-backward))
  :config
  (add-to-list 'display-buffer-alist
	       '("\*vterm\*"
		 (display-buffer-in-side-window)
		 (window-height . 0.3)
		 (side . bottom)
		 (slot . 0)))
  )

(provide 'funmacs-vterm)

;;; funmacs-vterm.el ends here
