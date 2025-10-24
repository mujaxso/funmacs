;;; funmacs-org-modern.el --- Funamcs: Modern Org styling module -*- lexical-binding: t; -*-

;; Depends: org, org-agenda, org-modern
;; Provides: funmacs-org-modern

;;; Commentary:
;; Polished, modern Org buffer styling and Org Agenda prettification using org-modern.

;;; Code:

;; org modern 
(use-package org-modern
  :ensure t
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)
	 ))

(provide 'funmacs-org-modern)

;;; funmacs-org-modern.el ends here
