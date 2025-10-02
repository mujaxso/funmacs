;;; funmacs-git.el --- magit -*- lexical-binding: t; -*-

;;; Commentary:
;; magit as git ui.

;;; Code:

(use-package magit
  :defer t
  :commands (magit-status magit-log))

(provide 'funmacs-git)

;;; funmacs-git.el ends here
