;;; funmacs-meow.el --- Meow modal config -*- lexical-binding: t; -*-

;;; Commentary:
;; This module provides a full Meow modal editing setup for Funmacs using use-package.

;;; Code:

(use-package meow
  :ensure t
  :demand t
  :config
  ;; Cheatsheet layout
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  ;; Cursor and mode-line feedback
  (setq meow-cursor-type-normal 'hbar
        meow-cursor-type-insert '(bar . 1)
        meow-cursor-type-motion 'hollow
        meow-use-cursor-position-hack t
        meow-use-clipboard t
        meow-mode-state-list
        '((normal . " Ⓝ ")
          (insert . " Ⓘ ")
          (keypad . " Ⓚ ")
          (motion . " Ⓜ ")))

  ;; Leader key (SPC)
  (meow-leader-define-key
   '("?" . meow-cheatsheet)
   '("f" . find-file)
   '("b" . switch-to-buffer)
   '("k" . kill-buffer)
   '("s" . save-buffer)
   '("x" . execute-extended-command)
   '("w" . other-window)
   '("p" . project-find-file)
   '("g" . goto-line)
   '("r" . query-replace)
   '("SPC" . execute-extended-command))

  ;; Normal state
  (meow-normal-define-key
   ;; Motions
   '("h" . meow-left)
   '("j" . meow-next)
   '("k" . meow-prev)
   '("l" . meow-right)
   '("H" . meow-left-expand)
   '("J" . meow-next-expand)
   '("K" . meow-prev-expand)
   '("L" . meow-right-expand)
   ;; Words
   '("w" . meow-next-word)
   '("b" . meow-back-word)
   '("e" . meow-next-end)
   ;; Object selections
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   ;; Insert / Append
   '("i" . meow-insert)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("I" . meow-open-above)
   ;; Editing
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("c" . meow-change)
   '("y" . meow-save)
   '("p" . meow-yank)
   '("P" . meow-sync-grab)
   ;; Selection / Block
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("v" . meow-visit)
   '("V" . meow-line)
   '("m" . meow-join)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   ;; Navigation
   '("gg" . (lambda () (interactive) (goto-char (point-min))))
   '("G"  . (lambda () (interactive) (goto-char (point-max))))
   ;; Search
   '("n" . meow-search)
   '("t" . meow-till)
   '(";" . meow-reverse)
   ;; Undo/redo
   '("u" . vundo)       
   '("U" . meow-undo)
   ;; Utility
   '("q" . meow-quit)
   '("'" . repeat)
   '("<escape>" . ignore))

  ;; Motion state — read-only navigation
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("h" . meow-left)
   '("l" . meow-right)
   '(";" . meow-reverse)
   '("n" . meow-search)
   '("q" . quit-window))

  ;; Insert state
  (define-key meow-insert-state-keymap (kbd "C-g") 'meow-cancel-selection)
  (define-key meow-insert-state-keymap (kbd "<escape>") 'meow-normal-mode)

  ;; Numeric expansions
  (meow-define-keys 'normal
    '("0" . meow-expand-0)
    '("1" . meow-expand-1)
    '("2" . meow-expand-2)
    '("3" . meow-expand-3)
    '("4" . meow-expand-4)
    '("5" . meow-expand-5)
    '("6" . meow-expand-6)
    '("7" . meow-expand-7)
    '("8" . meow-expand-8)
    '("9" . meow-expand-9))

  (delete-selection-mode 1)
  (meow-global-mode 1)
  (message "Funmacs: Meow global mode enabled."))

(provide 'funmacs-meow)
;;; funmacs-meow.el ends here
