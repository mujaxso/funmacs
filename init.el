;;; init.el --- Funmacs main entry -*- lexical-binding: t; -*-

(defvar funmacs-dir (file-name-directory load-file-name))
(defvar funmacs-core-dir (expand-file-name "core" funmacs-dir))
(defvar funmacs-modules-dir (expand-file-name "modules" funmacs-dir))

(add-to-list 'load-path funmacs-core-dir)
(add-to-list 'load-path funmacs-modules-dir)

;; Core
(require 'funmacs-packages)
(require 'funmacs-use-package)
(require 'funmacs-settings)
(require 'funmacs-emacs)
(require 'funmacs-backup)
(require 'funmacs-theme)
(require 'funmacs-git)
(require 'funmacs-font)
(require 'funmacs-treesitter)
(require 'funmacs-whichkey)
(require 'funmacs-vundo)
(require 'funmacs-eglot)

;; UI
(add-to-list 'load-path (expand-file-name "ui" funmacs-modules-dir))
(require 'funmacs-modeline)
(require 'funmacs-nerd-icons)

;; AI
(add-to-list 'load-path (expand-file-name "ai" funmacs-modules-dir))
(require 'funmacs-aidermacs)

;; Completion
(add-to-list 'load-path (expand-file-name "completion" funmacs-modules-dir))
(require 'funmacs-format)

;; Vertico comletion
(add-to-list 'load-path (expand-file-name "completion/vertico" funmacs-modules-dir))
(require 'funmacs-vertico)
(require 'funmacs-orderless)
(require 'funmacs-embark)

;; Corfu completion
(add-to-list 'load-path (expand-file-name "completion/corfu" funmacs-modules-dir))
(require 'funmacs-corfu)
(require 'funmacs-cape)
(require 'funmacs-tempel)

;; Lang modules
(add-to-list 'load-path (expand-file-name "lang" funmacs-modules-dir))
(require 'funmacs-c)
(require 'funmacs-cpp)
(require 'funmacs-python)
(require 'funmacs-javascript)
(require 'funmacs-typescript)
(require 'funmacs-html)
(require 'funmacs-css)
(require 'funmacs-json)
(require 'funmacs-go)
(require 'funmacs-rust)
(require 'funmacs-nix)
(require 'funmacs-react)
(require 'funmacs-vue)
(require 'funmacs-svelte)
(require 'funmacs-tailwind)
(require 'funmacs-markdown)

(message "✅ Funmacs full config loaded!")
(provide 'funmacs-init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(aidermacs apheleia cape corfu doom-modeline embark-consult
	       lsp-tailwindcss magit markdown-preview-mode
	       modus-themes nerd-icons-completion nerd-icons-corfu
	       nerd-icons-dired nix-mode orderless svelte-mode
	       tempel-collection vertico vue-mode vundo)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
