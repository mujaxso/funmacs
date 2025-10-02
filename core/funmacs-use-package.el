;;; funmacs-use-package.el --- use-package bootstrap -*- lexical-binding: t; -*-
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
(provide 'funmacs-use-package)
