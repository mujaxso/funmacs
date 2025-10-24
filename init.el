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
;;(require 'funmacs-eat)
(require 'funmacs-vterm)

;; UI
(add-to-list 'load-path (expand-file-name "ui" funmacs-modules-dir))
(require 'funmacs-modeline)
(require 'funmacs-nerd-icons)
(require 'funmacs-ligature)
(require 'funmacs-dashboard)

;; ORG
(add-to-list 'load-path (expand-file-name "org" funmacs-modules-dir))
(require 'funmacs-org)
(require 'funmacs-org-modern)
(require 'funmacs-agenda)
(require 'funmacs-todo)
(require 'funmacs-capture)
(require 'funmacs-superstar)

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
(require 'funmacs-consult)
(require 'funmacs-marginalia)

;; Corfu completion
(add-to-list 'load-path (expand-file-name "completion/corfu" funmacs-modules-dir))
(require 'funmacs-corfu)
(require 'funmacs-cape)
(require 'funmacs-tempel)

;; Lang modules
(add-to-list 'load-path (expand-file-name "lang" funmacs-modules-dir))
(require 'funmacs-c)
(require 'funmacs-cpp)
(require 'funmacs-zig)
(require 'funmacs-shell)
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
(require 'funmacs-toml)

(message "✅ Funmacs full config loaded!")
(provide 'funmacs-init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(aidermacs apheleia async bind-key calibredb cape cl-generic cl-lib
	       closql compat cond-let connection consult corfu
	       csharp-mode dash dashboard dictionary doom-modeline
	       edit-indirect editorconfig eglot eldoc elfeed
	       elixir-ts-mode emacsql embark embark-consult erc esxml
	       exec-path-from-shell external-completion f faceup
	       flymake forge general ghub git-gutter gptel
	       heex-ts-mode hl-todo ht htmlize idlwave impatient-mode
	       jsonrpc language-detection less-css-mode let-alist
	       ligature link llama lsp-mode lsp-tailwindcss lv magit
	       magit-org-todos magit-prime magit-section magit-todos
	       map marginalia markdown-mode markdown-preview-mode meow
	       mmm-mode modus-themes nadvice nerd-icons
	       nerd-icons-completion nerd-icons-corfu nerd-icons-dired
	       nix-mode nov ntlm orderless org org-appear org-modern
	       org-superstar pcre2el peg poly-org polymode project
	       python reformatter request s seq shrface shrink-path
	       simple-httpd so-long soap-client spinner ssass-mode
	       svelte-mode svg tempel tempel-collection track-changes
	       tramp transient treepy use-package valign verilog-mode
	       vertico vterm vterm-toggle vue-html-mode vue-mode vundo
	       wallabag wallpaper web-server websocket which-key
	       window-tool-bar with-editor xref yaml zig-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9))))
 '(markdown-header-face-1 ((t (:height 1.8 :weight extra-bold :foreground "#79c0ff"))))
 '(markdown-header-face-2 ((t (:height 1.4 :weight extra-bold :foreground "#79c0ff"))))
 '(markdown-header-face-3 ((t (:height 1.2 :weight extra-bold :foreground "#79c0ff"))))
 '(markdown-header-face-4 ((t (:height 1.15 :weight bold :foreground "#79c0ff"))))
 '(markdown-header-face-5 ((t (:height 1.1 :weight bold :foreground "#79c0ff"))))
 '(markdown-header-face-6 ((t (:height 1.05 :weight semi-bold :foreground "#79c0ff"))))
 '(org-document-title ((t (:height 1.5 :weight bold :underline nil))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.3 :weight bold))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.2 :weight bold))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1 :weight semi-bold))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.05 :weight semi-bold))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(org-superstar-first ((t (:inherit org-warning :foreground "#ECBE7B"))))
 '(org-superstar-header-bullet ((t (:inherit default :foreground "#ff6c6b" :weight bold))))
 '(org-superstar-item ((t (:inherit default :foreground "#51afef"))))
 '(org-superstar-leading ((t (:inherit default :foreground "#3a3f4b")))))
