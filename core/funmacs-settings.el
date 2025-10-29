;;; funmacs-settings.el --- Funmacs settings -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic settings: disable bell sound and enable line numbers.

;;; Code:

;; Disable bell sound
(setq ring-bell-function 'ignore)

;; Enable relative line numbers in programming modes only
(defun funmacs-enable-line-numbers ()
  "Enable relative line numbers for programming modes."
  (setq display-line-numbers-type 'relative)
  (display-line-numbers-mode 1))

(add-hook 'prog-mode-hook #'funmacs-enable-line-numbers)

;; autocomplete paired brackets
(electric-pair-mode 1)

;; auto update file from disk
(global-auto-revert-mode 1)

;; UTF-8 as defaul
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; load path from shell
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))


(provide 'funmacs-settings)
;;; funmacs-settings.el ends here
