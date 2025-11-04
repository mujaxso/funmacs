;;; funmacs-corfu.el --- Corfu -*- lexical-binding: t; -*-

;; Requires: corfu, eglot, eldoc, eldoc-mouse, posframe, nerd-icons-completion

;;; Code:

;; --- Corfu setup -------------------------------------------------------
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary nil)
  (corfu-scroll-margin 5)
  (completion-styles '(orderless basic))
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode)
  :config
  (corfu-history-mode)
  (corfu-indexed-mode)
  (corfu-popupinfo-mode)
  (use-package nerd-icons-completion
    :ensure t
    :config
    (nerd-icons-completion-mode 1))
  :bind
  (:map corfu-map
	("C-n" . corfu-next)
	("C-p" . corfu-previous)
	("<escape>" . corfu-quit)
	("<return>" . corfu-insert)
	("M-d" . corfu-show-documentation)
	("M-l" . corfu-show-location)))

(provide 'funmacs-corfu)
;;; funmacs-corfu.el ends here
