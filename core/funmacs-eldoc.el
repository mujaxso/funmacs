;;; funmacs-eldoc.el --- Eldoc and Eldoc-Mouse popup hover configuration -*- lexical-binding: t; -*-

;; Requires: eglot, eldoc, eldoc-mouse, posframe
;; Provides: persistent hover documentation popups with mouse movement detection

;;; Code:

(use-package eldoc
  :ensure t
  :custom
  (eldoc-idle-delay 0.2)
  :init
  (global-eldoc-mode 1))

(use-package posframe
  :ensure t)

(use-package eldoc-mouse
  :ensure t
  :hook (prog-mode . eldoc-mouse-mode)
  :custom
  (eldoc-mouse-hover-delay 0.15))

(provide 'funmacs-eldoc)
;;; funmacs-eldoc.el ends here
