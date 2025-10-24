;;; funmacs-font.el --- fonts -*- lexical-binding: t; -*-
;; Use Nerd font if available, fallback silently otherwise

;;; Commentary:
;; font settings

;;; code

(when (member "JetBrainsMono Nerd Font" (font-family-list))
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 120)
  (set-face-attribute 'fixed-pitch nil :family "JetBrainsMono Nerd Font" :height 1.0)
  (set-face-attribute 'variable-pitch nil :family "Inter" :height 1.0))



(provide 'funmacs-font)

;;; funmacs-font.el ends here
