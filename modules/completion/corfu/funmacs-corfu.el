;;; funmacs-corfu.el -*- lexical-binding: t; -*-

;;; Commentary:
;; corfu completion ui

;;; code

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode)
  ;; Enable history
  (corfu-history-mode t)
  ;; Enable indexed
  (corfu-indexed-mode t)
  ;; ;; Enable info
  ;; (corfu-info-mode t)
  ;; Enable popupinfo
  (corfu-popupinfo-mode t)
  ;; ;; Enable quick
  ;; (corfu-quick-mode t)
  ;; Aggressive completion, cheap prefix filtering.
  (setq corfu-auto t
        corfu-quit-no-match 'separator
        corfu-auto-delay 0
        corfu-auto-prefix 0
        completion-styles '(basic))
  :bind (:map corfu-map
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)
	      ("<escape>" . corfu-quit)
              ("<return>" . corfu-insert)
              ("M-d" . corfu-show-documentation)
              ("M-l" . corfu-show-location)
	      )
  ;;:hook
  ;; after init
  ;;(after-init . corfu-mode)
  ;; ;; yasnippet suggestion for lsp-mode
  ;; (eglot-managed-mode . funmacs/eglot-capf)
  ;; :config
  ;; ;; add suggestion for yasnippets when using eglot
  ;; (defun funmacs/eglot-capf ()
  ;;   (setq-local completion-at-point-functions
  ;; 		(list (cape-super-capf
  ;; 		       #'eglot-completion-at-point
  ;;                      (cape-company-to-capf #'company-yasnippet)))))
  )

(provide 'funmacs-corfu)

;;; funmacs-corfu.el ends here
