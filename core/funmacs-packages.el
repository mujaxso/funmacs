;;; funmacs-packages.el --- package config -*- lexical-binding: t; -*-

;;; Commentary:
;; package.el

;;; code

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(unless (bound-and-true-p package--initialized) (package-initialize))
(unless package-archive-contents (package-refresh-contents))

(provide 'funmacs-packages)

;;; funmacs-package.el ends here
