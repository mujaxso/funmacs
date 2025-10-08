;;; funmacs-corfu-ide.el --- Corfu IDE-style configuration -*- lexical-binding: t; -*-

;; Dependencies:
;; corfu, corfu-popupinfo, cape, marginalia

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                ;; Cycle candidates
  (corfu-auto t)                 ;; Auto completion
  (corfu-separator ?\s)          ;; Orderless separator
  (corfu-quit-at-boundary nil)   ;; Never quit automatically
  (corfu-scroll-margin 5)        ;; Scroll margin
  (completion-styles '(orderless basic)) ;; IDE-like fuzzy matching

  :init
  (global-corfu-mode)            ;; Enable Corfu everywhere
  (corfu-history-mode t)         ;; Remember completions
  (corfu-indexed-mode t)         ;; Index completions
  (corfu-popupinfo-mode t)       ;; Show documentation popup
  (setq corfu-auto-delay 0
        corfu-auto-prefix 1      ;; Trigger after 1 char
        corfu-quit-no-match 'separator)

  :bind (:map corfu-map
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)
              ("<escape>" . corfu-quit)
              ("<return>" . corfu-insert)
              ("M-d" . corfu-show-documentation)
              ("M-l" . corfu-show-location))

  :config
  ;; Add Nerd Font icons to completion candidates
  (use-package nerd-icons-completion
    :ensure t
    :config
    (nerd-icons-completion-mode 1))

  ;; Optional: integrate yasnippet completions
  ;; (use-package corfu-popupinfo
  ;;   :ensure t
  ;;   :config
  ;;   (corfu-popupinfo-mode +1))
  )

(provide 'funmacs-corfu)

;;; funmacs-corfu-ide.el ends here
