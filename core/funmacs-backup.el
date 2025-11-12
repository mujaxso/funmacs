;;; funmacs-backup.el --- backups -*- lexical-binding: t; -*-

;;; Commentary:
;; backup files settings using use-package

;;; code

(defvar funmacs-tmp-dir (expand-file-name "tmp/" user-emacs-directory))
(defvar funmacs-backup-dir (expand-file-name "backups/" funmacs-tmp-dir))
(defvar funmacs-autosave-dir (expand-file-name "autosaves/" funmacs-tmp-dir))

(use-package files
  :ensure nil
  :init
  (dolist (d (list funmacs-tmp-dir funmacs-backup-dir funmacs-autosave-dir))
    (unless (file-directory-p d) (make-directory d t)))
  :custom
  (backup-directory-alist `(("." . ,funmacs-backup-dir)))
  (auto-save-file-name-transforms `((".*" ,funmacs-autosave-dir t)))
  (create-lockfiles nil)
  :config
  (defun funmacs-clean-tmp () 
    (interactive)
    (when (yes-or-no-p "Really delete Funmacs tmp/ backups and autosaves? ")
      (delete-directory funmacs-tmp-dir t) 
      (make-directory funmacs-backup-dir t)
      (make-directory funmacs-autosave-dir t)
      (message "Funmacs: cleaned tmp/")))
  
  (defun funmacs-reset-session () 
    (interactive) 
    (funmacs-clean-tmp) 
    (dolist (b (buffer-list)) 
      (with-current-buffer b 
        (when (boundp 'buffer-undo-list) 
          (setq buffer-undo-list nil)))) 
    (message "Funmacs: session reset.")))

(provide 'funmacs-backup)
;;; funmacs-backup.el ends here
