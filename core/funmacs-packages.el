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

(defvar funmacs/package-last-upgrade-time 0
  "Timestamp of the last time built-in packages were upgraded.")

(defvar funmacs/package-upgrade-interval (* 7 24 60 60)
  "Interval in seconds between automatic upgrades (default: 7 days).")

(defun funmacs/package-upgrade-builtins ()
  "Upgrade all built-in packages if upgrade interval has passed."
  (when (> (- (float-time) funmacs/package-last-upgrade-time) funmacs/package-upgrade-interval)
    (message "📦 Upgrading built-in packages...")
    ;; Only refresh if needed
    (unless package-archive-contents
      (package-refresh-contents))
    (dolist (pkg (mapcar #'car package-archive-contents))
      (when (package-installed-p pkg)
        (package-install pkg)))
    (setq funmacs/package-last-upgrade-time (float-time))
    (message "✅ Built-in packages upgraded.")))

(add-hook 'emacs-startup-hook #'funmacs/package-upgrade-builtins)

(provide 'funmacs-packages)

;;; funmacs-package.el ends here
