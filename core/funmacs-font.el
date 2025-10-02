;;; funmacs-font.el --- fonts -*- lexical-binding: t; -*-
;; Use Nerd font if available, fallback silently otherwise
(when (member "FiraCode Nerd Font" (font-family-list))
  (set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 120))
(provide 'funmacs-font)
