;;; init.el --- Funmacs main entry -*- lexical-binding: t; -*-

;;; Commentary:
;; Funmacs configuration using use-package for modern, modular Emacs setup.

;;; Code:

;; Set up package archives and initialize use-package
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; =============================================================================
;; CORE SETTINGS
;; =============================================================================

;; Basic Emacs settings
(use-package emacs
  :ensure nil
  :custom
  (ring-bell-function 'ignore)
  (set-default-coding-systems 'utf-8)
  (prefer-coding-system 'utf-8)
  :config
  (electric-pair-mode 1)
  (global-auto-revert-mode 1)
  (mouse-wheel-mode 1)
  (setq track-mouse t))

;; Line numbers for programming modes
(defun funmacs-enable-line-numbers ()
  "Enable relative line numbers for programming modes."
  (setq display-line-numbers-type 'relative)
  (display-line-numbers-mode 1))

(use-package prog-mode
  :ensure nil
  :hook (prog-mode . funmacs-enable-line-numbers))

;; Shell path setup
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; =============================================================================
;; BACKUP SYSTEM
;; =============================================================================

(defvar funmacs-tmp-dir (expand-file-name "tmp/" user-emacs-directory))
(defvar funmacs-backup-dir (expand-file-name "backups/" funmacs-tmp-dir))
(defvar funmacs-autosave-dir (expand-file-name "autosaves/" funmacs-tmp-dir))

(use-package files
  :ensure nil
  :init
  (dolist (d (list funmacs-tmp-dir funmacs-backup-dir funmacs-autosave-dir))
    (unless (file-directory-p d) (make-directory d t)))
  :custom
  (backup-directory-alist `(("." . ,funmacs-backup-dir)))
  (auto-save-file-name-transforms `((".*" ,funmacs-autosave-dir t)))
  (create-lockfiles nil))

;; =============================================================================
;; THEME AND APPEARANCE
;; =============================================================================

(use-package modus-themes
  :demand t
  :custom
  (modus-themes-italic-constructs t)
  :config
  (load-theme 'modus-vivendi-tinted t))

;; Font configuration
(use-package emacs
  :ensure nil
  :init
  (when (member "JetBrainsMono Nerd Font" (font-family-list))
    (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 120)
    (set-face-attribute 'fixed-pitch nil :family "JetBrainsMono Nerd Font" :height 1.0)
    (set-face-attribute 'variable-pitch nil :family "Inter" :height 1.0)))

;; =============================================================================
;; GIT INTEGRATION
;; =============================================================================

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :custom
  (git-commit-summary-max-length 50)
  (git-commit-fill-column 72)
  :config
  (add-hook 'after-save-hook #'magit-after-save-refresh-status t))

(use-package forge
  :after magit)

(use-package magit-todos
  :after magit
  :hook (magit-mode . magit-todos-mode)
  :custom
  (magit-todos-keywords '("TODO" "FIXME" "HACK" "NOTE")))

(use-package magit-org-todos
  :after magit
  :config
  (magit-org-todos-autoinsert))

(use-package magit-prime
  :after magit
  :config
  (magit-prime-mode 1))

(use-package git-gutter
  :init (global-git-gutter-mode +1)
  :custom
  (git-gutter:update-interval 2))

;; =============================================================================
;; TREE-SITTER
;; =============================================================================

(use-package treesit
  :ensure nil
  :custom
  (treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (java "https://github.com/tree-sitter/tree-sitter-java")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
     (rust "https://github.com/tree-sitter/tree-sitter-rust")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
     (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")
     (toml "https://github.com/tree-sitter-grammars/tree-sitter-toml")
     (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
     (zig "https://github.com/tree-sitter-grammars/tree-sitter-zig")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
     (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
     (nix "https://github.com/nix-community/tree-sitter-nix")))
  (major-mode-remap-alist
   '((bash-mode . bash-ts-mode)
     (sh-mode . bash-ts-mode)
     (c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (c-sharp-mode . c-sharp-ts-mode)
     (csharp-mode . csharp-ts-mode)
     (css-mode . css-ts-mode)
     (html-mode . html-ts-mode)
     (mhtml-mode . html-ts-mode)
     (javascript-mode . js-ts-mode)
     (js-mode . js-ts-mode)
     (js2-mode . js-ts-mode)
     (typescript-mode . typescript-ts-mode)
     (json-mode . json-ts-mode)
     (yaml-mode . yaml-ts-mode)
     (toml-mode . toml-ts-mode)
     (conf-toml-mode . toml-ts-mode)
     (python-mode . python-ts-mode)
     (ruby-mode . ruby-ts-mode)
     (enh-ruby-mode . ruby-ts-mode)
     (lua-mode . lua-ts-mode)
     (rust-mode . rust-ts-mode)
     (rustic-mode . rust-ts-mode)
     (go-mode . go-ts-mode)
     (zig-mode . zig-ts-mode)
     (elixir-mode . elixir-ts-mode)
     (java-mode . java-ts-mode)
     (cmake-mode . cmake-ts-mode)
     (dockerfile-mode . dockerfile-ts-mode)
     (sql-mode . sql-ts-mode))))

;; =============================================================================
;; COMPLETION AND EDITING
;; =============================================================================

;; Which-key for discoverable keybindings
(use-package which-key
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.5))

;; Vundo for visual undo
(use-package vundo
  :bind (("C-x u" . funmacs/vundo-open)
         (:map meow-normal-state-keymap ("u" . funmacs/vundo-open)))
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display t)
  (undo-limit 800000)
  (undo-strong-limit 12000000)
  (undo-outer-limit 120000000)
  :config
  (defun funmacs/vundo-open ()
    "Open vundo in a non-dedicated window."
    (interactive)
    (let ((display-buffer-alist
           '(("\\*vundo-tree\\*"
              (display-buffer-in-side-window)
              (side . right)
              (window-width . 0.3)
              (dedicated . nil)))))
      (vundo))))

;; Eglot for LSP
(use-package eglot
  :defer t)

;; Meow for modal editing
(use-package meow
  :config
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty
        meow-cursor-type-normal 'hbar
        meow-cursor-type-insert '(bar . 1)
        meow-cursor-type-motion 'hollow
        meow-use-cursor-position-hack t
        meow-use-clipboard t
        meow-mode-state-list
        '((normal . " Ⓝ ")
          (insert . "Ⓘ ")
          (keypad . "Ⓚ ")
          (motion . "Ⓜ ")))
  
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

  (meow-normal-define-key
   '("h" . meow-left)
   '("j" . meow-next)
   '("k" . meow-prev)
   '("l" . meow-right)
   '("H" . meow-left-expand)
   '("J" . meow-next-expand)
   '("K" . meow-prev-expand)
   '("L" . meow-right-expand)
   '("w" . meow-next-word)
   '("b" . meow-back-word)
   '("e" . meow-next-end)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("i" . meow-insert)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("I" . meow-open-above)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("c" . meow-change)
   '("y" . meow-save)
   '("p" . meow-yank)
   '("P" . meow-sync-grab)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("v" . meow-visit)
   '("V" . meow-line)
   '("m" . meow-join)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("gg" . (lambda () (interactive) (goto-char (point-min))))
   '("G"  . (lambda () (interactive) (goto-char (point-max))))
   '("n" . meow-search)
   '("t" . meow-till)
   '(";" . meow-reverse)
   '("u" . vundo)
   '("U" . meow-undo)
   '("q" . meow-quit)
   '("'" . repeat)
   '("<escape>" . ignore))

  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("h" . meow-left)
   '("l" . meow-right)
   '(";" . meow-reverse)
   '("n" . meow-search)
   '("q" . quit-window))

  (define-key meow-insert-state-keymap (kbd "C-g") 'meow-cancel-selection)
  (define-key meow-insert-state-keymap (kbd "<escape>") 'meow-normal-mode)

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
  (meow-global-mode 1))

;; Leader key bindings
(use-package general
  :after meow
  :config
  (general-create-definer funmacs/leader
    :states '(normal motion visual)
    :keymaps 'override
    :prefix "SPC")

  (funmacs/leader
   "f"  '(:ignore t :wk "file")
   "f f" '(find-file :wk "find file")
   "f r" '(recentf-open-files :wk "recent files")
   "f s" '(save-buffer :wk "save file")
   "f S" '(write-file :wk "save as")
   "b"  '(:ignore t :wk "buffer")
   "b b" '(switch-to-buffer :wk "switch buffer")
   "b k" '(kill-buffer :wk "kill buffer")
   "b n" '(next-buffer :wk "next buffer")
   "b p" '(previous-buffer :wk "previous buffer")
   "b l" '(ibuffer :wk "buffer list")
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
   "y"  '(:ignore t :wk "clipboard")
   "y y" '(clipboard-kill-ring-save :wk "copy")
   "y p" '(clipboard-yank :wk "paste")
   "y x" '(clipboard-kill-region :wk "cut")
   "h"  '(:ignore t :wk "help")
   "h k" '(describe-key :wk "describe key")
   "h f" '(describe-function :wk "describe function")
   "h v" '(describe-variable :wk "describe variable")
   "h m" '(describe-mode :wk "describe mode")
   "h b" '(describe-bindings :wk "describe bindings")
   "t"  '(:ignore t :wk "tools")
   "t m" '(man :wk "man page")
   "t e" '(eval-buffer :wk "eval buffer")
   "t r" '(recompile :wk "recompile")
   "t g" '(magit-status :wk "magit status")
   "t s" '(eshell :wk "shell")
   "t v" '(vundo :wk "vundo")))

;; Eldoc for documentation
(use-package eldoc
  :custom
  (eldoc-idle-delay 0.2)
  :config
  (global-eldoc-mode 1))

(use-package eldoc-mouse
  :hook (prog-mode . eldoc-mouse-mode)
  :custom
  (eldoc-mouse-hover-delay 0.15))

;; =============================================================================
;; TERMINAL EMULATION
;; =============================================================================

;; Vterm configuration
(use-package vterm
  :commands (vterm vterm-other-window)
  :custom
  (vterm-max-scrollback 10000)
  (vterm-buffer-name-string "vterm: %s")
  :config
  (defun funmacs-vterm-project ()
    "Open a vterm in the project root."
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (vterm-other-window))))

(use-package vterm-toggle
  :bind (("C-c t" . vterm-toggle)
         :map vterm-mode-map
         ("<C-return>" . vterm-toggle-insert-cd)
         ("s-n" . vterm-toggle-forward)
         ("s-p" . vterm-toggle-backward))
  :config
  (add-to-list 'display-buffer-alist
               '("\*vterm\*"
                 (display-buffer-in-side-window)
                 (window-height . 0.3)
                 (side . bottom)
                 (slot . 0))))

;; Eat terminal (alternative)
(use-package eat
  :defer t
  :commands (eat eat-other-window eat-project)
  :custom
  (eat-shell "/bin/bash")
  (eat-term-maximum-scrollback 10000)
  :bind (("C-c t" . #'eat)
         ("C-c T" . #'eat-other-window))
  :config
  (defun funmacs-eat-project ()
    "Open an EAT terminal in the project root."
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (eat-other-window)))
  (global-set-key (kbd "C-c p t") #'funmacs-eat-project))

;; =============================================================================
;; FINAL SETUP
;; =============================================================================

(message "✅ Funmacs full config loaded!")

(provide 'funmacs-init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(aidermacs apheleia cape corfu dashboard doom-modeline eat eldoc-box
	       eldoc-mouse embark-consult exec-path-from-shell forge
	       general git-gutter ligature lsp-tailwindcss
	       magit-org-todos magit-prime magit-todos marginalia meow
	       modus-themes nerd-icons-completion nerd-icons-corfu
	       nerd-icons-dired nix-mode orderless org-modern
	       svelte-mode tempel-collection valign vertico
	       vterm-toggle vue-mode vundo zig-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
