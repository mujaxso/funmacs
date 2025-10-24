;;; funmacs-leader.el --- Funmacs leader key configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This module provides a comprehensive leader key setup for Funmacs using `general.el`.
;; Features:
;; - Fully compatible with Meow modal editing (normal, motion, visual states)
;; - Organized prefixes for files, buffers, windows, clipboard, help, and tools
;; - Which-key hints for discoverability
;; - Optional global prefix (C-SPC) for commands outside Meow
;; - Easily expandable for future modules (projects, git, etc.)
;;
;; Usage:
;; Load this module and Meow first. SPC will serve as the main leader key.

;;; Code:

(use-package general
  :after meow
  :config
  ;; Create Funmacs leader key
  (general-create-definer funmacs/leader
    :states '(normal motion visual)
    :keymaps 'override
    :prefix "SPC")

  ;; Leader key bindings
  (funmacs/leader
   ;; Files
   "f"  '(:ignore t :wk "file")
   "f f" '(find-file :wk "find file")
   "f r" '(recentf-open-files :wk "recent files")
   "f s" '(save-buffer :wk "save file")
   "f S" '(write-file :wk "save as")

   ;; Buffers
   "b"  '(:ignore t :wk "buffer")
   "b b" '(switch-to-buffer :wk "switch buffer")
   "b k" '(kill-buffer :wk "kill buffer")
   "b n" '(next-buffer :wk "next buffer")
   "b p" '(previous-buffer :wk "previous buffer")
   "b l" '(ibuffer :wk "buffer list")

   ;; Windows
   "w"  '(:ignore t :wk "window")
   "w w" '(other-window :wk "other window")
   "w s" '(split-window-below :wk "split below")
   "w v" '(split-window-right :wk "split right")
   "w d" '(delete-window :wk "delete window")
   "w m" '(delete-other-windows :wk "maximize window")
   "w h" '(windmove-left :wk "move left")
   "w j" '(windmove-down :wk "move down")
   "w k" '(windmove-up :wk "move up")
   "w l" '(windmove-right :wk "move right")

   ;; Clipboard
   "y"  '(:ignore t :wk "clipboard")
   "y y" '(clipboard-kill-ring-save :wk "copy")
   "y p" '(clipboard-yank :wk "paste")
   "y x" '(clipboard-kill-region :wk "cut")

   ;; Help
   "h"  '(:ignore t :wk "help")
   "h k" '(describe-key :wk "describe key")
   "h f" '(describe-function :wk "describe function")
   "h v" '(describe-variable :wk "describe variable")
   "h m" '(describe-mode :wk "describe mode")
   "h b" '(describe-bindings :wk "describe bindings")

   ;; Tools / Utilities
   "t"  '(:ignore t :wk "tools")
   "t m" '(man :wk "man page")
   "t e" '(eval-buffer :wk "eval buffer")
   "t r" '(recompile :wk "recompile")
   "t g" '(magit-status :wk "magit status")  ;; optional, if you use magit
   "t s" '(eshell :wk "shell")
   "t v" '(vundo :wk "vundo")))

(provide 'funmacs-leader)
;;; funmacs-leader.el ends here
