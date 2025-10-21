;;; funmacs-org.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Fast, ergonomic Org defaults with lazy loading and large-file safeguards.

;;; Code:

;; Directory setup
(defvar funmacs/org-dir (expand-file-name "~/org/")
  "Primary Org directory.")
(setq org-directory funmacs/org-dir)
(setq org-default-notes-file (expand-file-name "notes.org" org-directory))

;; Agenda files: recurse .org, ignore backups/hidden
(setq org-agenda-files
      (let ((dir org-directory))
        (when (file-directory-p dir)
          (directory-files-recursively dir "\\`[^.].*\\.org\\'"))))

;; Skip per-file startup when agenda first visits files
(setq org-agenda-inhibit-startup t)  ;; speeds agenda on first open [manual tip]

;; Global keys: these are autoloaded; no need to (require 'org) at init
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;; Core behavior
(setq org-log-done 'time)            ;; timestamp DONE
(setq org-return-follows-link t)     ;; RET follows links [links manual]

;; Visuals
(setq org-hide-emphasis-markers t)   ;; hide *bold* markers
(setq org-startup-folded t)          ;; start folded (fast on big files)

;; Prefer startup variable for indent; avoid always-on hook cost
(setq org-startup-indented t)        ;; equivalent to org-indent-mode at startup [Org Indent manual]

;; Large-file guard: dampen expensive minors in huge buffers
(defun funmacs/org-large-buffer-setup ()
  "Tune Org features for large buffers."
  (when (and (derived-mode-p 'org-mode)
             (> (buffer-size) 500000)) ;; ~500KB threshold, adjust as needed
    ;; Back off costly visuals on very large files
    (when (bound-and-true-p org-indent-mode)
      (org-indent-mode -1))
    (setq-local org-hide-emphasis-markers nil)
    (setq-local org-fold-core-style 'overlays))) ;; simpler folding if available
(add-hook 'org-mode-hook #'funmacs/org-large-buffer-setup)

;; Editing in src blocks
(setq org-src-fontify-natively t)    ;; needed for native TAB in blocks [doc]
(setq org-src-tab-acts-natively t)   ;; TAB delegates to language mode [doc]
;; If TAB sluggish in huge blocks, consider toggling per-mode or via a size check.

;; Soft-wrap and visual indent when not huge
(add-hook 'org-mode-hook #'visual-line-mode)

(provide 'funmacs-org)
;;; funmacs-org.el ends here
