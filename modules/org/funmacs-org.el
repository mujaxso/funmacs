;;; funmacs-org.el -*- lexical-binding: t; -*-

;;; Commentary:
;; A GNU Emacs major mode for keeping notes, authoring documents, computational notebooks, literate programming, maintaining to-do lists, planning projects, and more — in a fast and effective plain text system.

;;; Code:

;; Load Org mode
(require 'org)

;; Directory setup
(setq org-directory (expand-file-name "~/org/"))
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-agenda-files '("~/org/"))

;; Global keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Log completion time
(setq org-log-done 'time)

;; Follow links with RET
(setq org-return-follows-link t)

;; Visual improvements
(setq org-hide-emphasis-markers t)
(setq org-startup-folded t)

;; Better indentation
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

;; Code block syntax highlighting
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(provide 'funmacs-org)
;;; funmacs-org.el ends here
