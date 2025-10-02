;;; funmacs-tempel.el --- Templates via Tempel -*- lexical-binding: t; -*-

;;; Commentary:
;; Tempel is a simple template/snippet system for Emacs.
;; Provides a lightweight alternative to yasnippet.
;; Works well with Corfu and CAPFs.

;;; Code:

(use-package tempel
  :ensure t
  :bind (("M-+" . tempel-complete) ;; expand or insert template
         ("M-*" . tempel-insert))  ;; prompt for template
  :init
  ;; Add Tempel to completion-at-point
  (add-to-list 'completion-at-point-functions #'tempel-complete)

  ;; Set the template file location
  (setq tempel-path (expand-file-name "templates.eld"
                                      (file-name-directory load-file-name))))

(use-package tempel-collection
  :ensure t
  :after tempel)

(provide 'funmacs-tempel)

;;; funmacs-tempel.el ends here
