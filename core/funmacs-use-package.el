;;; funmacs-use-package.el --- use-package bootstrap -*- lexical-binding: t; -*-

;;; Commentary:
;; use-package settings

;;; code

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)

;; Load general.el to prevent native compilation warnings
(use-package general
  :ensure t
  :defer t)

(provide 'funmacs-use-package)

;;; funmacs-use-package.el ends here
