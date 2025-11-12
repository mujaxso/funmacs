;;; funmacs-treesitter.el -*- lexical-binding: t; -*-

;;; Commentary:
;; tree-sitter settings with verified working grammars only
;; Auto-installs missing grammars on first launch

;;; Code:

(require 'cl-lib)

(use-package treesit
  :ensure nil
  :init
  ;; Fix git authentication issues on NixOS
  (setenv "GIT_ASKPASS" "")
  (setenv "SSH_ASKPASS" "")
  :custom
  (treesit-language-source-alist
   '(;; Core Languages - Official tree-sitter repos (100% verified)
     (bash "https://github.com/tree-sitter/tree-sitter-bash")
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
     
     ;; tree-sitter-grammars organization (verified)
     (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
     (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")
     (toml "https://github.com/tree-sitter-grammars/tree-sitter-toml")
     (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
     (zig "https://github.com/tree-sitter-grammars/tree-sitter-zig")
     
     ;; Community maintained (tested and working)
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
     (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
     (nix "https://github.com/nix-community/tree-sitter-nix")))
  :config
  (setq major-mode-remap-alist
        '(;; System & Shell
          (bash-mode . bash-ts-mode)
          (sh-mode . bash-ts-mode)
          
          ;; C Family
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (c-sharp-mode . c-sharp-ts-mode)
          (csharp-mode . csharp-ts-mode)
          
          ;; Web Development
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
          
          ;; Scripting Languages
          (python-mode . python-ts-mode)
          (ruby-mode . ruby-ts-mode)
          (enh-ruby-mode . ruby-ts-mode)
          (lua-mode . lua-ts-mode)
          
          ;; Systems Programming
          (rust-mode . rust-ts-mode)
          (rustic-mode . rust-ts-mode)
          (go-mode . go-ts-mode)
          (zig-mode . zig-ts-mode)
          
          ;; Functional Languages
          (elixir-mode . elixir-ts-mode)
          
          ;; JVM Languages
          (java-mode . java-ts-mode)
          
          ;; Build Systems & Config
          (cmake-mode . cmake-ts-mode)
          (dockerfile-mode . dockerfile-ts-mode)
          (nix-mode . nix-ts-mode)
          
          ;; Documentation & Markup
          (markdown-mode . markdown-ts-mode)
          (gfm-mode . markdown-ts-mode)
          
          ;; Other
          (sql-mode . sql-ts-mode)))

  (defun funmacs--compiler-available-p () 
  "Check if a C/C++ compiler is available in PATH."
  (or (executable-find "gcc") 
      (executable-find "clang") 
      (executable-find "cc")))

(defun funmacs--all-grammars-installed-p ()
  "Check if all grammars in treesit-language-source-alist are installed."
  (cl-every (lambda (grammar)
              (treesit-language-available-p (car grammar)))
            treesit-language-source-alist))

(defun funmacs-install-all-grammars ()
  "Install ALL tree-sitter grammars defined in treesit-language-source-alist.
This function installs grammars unconditionally without checking if they exist."
  (interactive)
  (if (funmacs--compiler-available-p)
      (let ((installed 0)
            (failed 0)
            (failed-langs '()))
        (message "[Funmacs] Starting installation of %d tree-sitter grammars..." 
                 (length treesit-language-source-alist))
        (dolist (grammar treesit-language-source-alist)
          (let ((lang (car grammar)))
            (condition-case err
                (progn
                  (message "[Funmacs] Installing grammar for %s..." lang)
                  (treesit-install-language-grammar lang)
                  (setq installed (1+ installed))
                  (message "[Funmacs] ✓ Successfully installed %s" lang))
              (error 
               (setq failed (1+ failed))
               (push lang failed-langs)
               (message "[Funmacs] ✗ Failed to install %s: %s" 
			lang (error-message-string err))))))
        (message "[Funmacs] ========================================")
        (message "[Funmacs] Installation complete: %d installed, %d failed" 
                 installed failed)
        (when failed-langs
          (message "[Funmacs] ⚠ Failed languages: %s" 
                   (mapconcat #'symbol-name (reverse failed-langs) ", ")))
        (message "[Funmacs] ========================================"))
    (user-error "[Funmacs] No C compiler found! Install gcc or clang first.")))

(defun funmacs-install-missing-grammars (&optional silent)
  "Install only missing tree-sitter grammars with detailed error reporting.
If SILENT is non-nil, only show messages when actually installing or on errors."
  (interactive)
  (if (funmacs--compiler-available-p)
      (let ((installed 0)
            (failed 0)
            (skipped 0)
            (failed-langs '()))
        (dolist (grammar treesit-language-source-alist)
          (let ((lang (car grammar)))
            (if (treesit-language-available-p lang)
                (progn
                  (setq skipped (1+ skipped))
                  (unless silent
                    (message "[Funmacs] ✓ Grammar for %s already installed" lang)))
              (condition-case err
                  (progn
                    (message "[Funmacs] Installing tree-sitter grammar for %s..." lang)
                    (treesit-install-language-grammar lang)
                    (setq installed (1+ installed))
                    (message "[Funmacs] ✓ Successfully installed %s" lang))
                (error 
                 (setq failed (1+ failed))
                 (push lang failed-langs)
                 (message "[Funmacs] ✗ Failed to install %s: %s" lang (error-message-string err)))))))
        ;; Only show summary if something was actually done or if not silent
        (when (or (> installed 0) (> failed 0) (not silent))
          (message "[Funmacs] ========================================")
          (message "[Funmacs] Installation complete: %d installed, %d failed, %d already present" 
                   installed failed skipped)
          (when failed-langs
            (message "[Funmacs] ⚠ Failed languages: %s" (mapconcat #'symbol-name (reverse failed-langs) ", ")))
          (message "[Funmacs] ========================================")))
    (unless silent
      (message "[Funmacs] No C compiler found; skipping tree-sitter installs."))))

;; Auto-install missing grammars on startup (silent if all present)
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (and (fboundp 'treesit-available-p)
                       (treesit-available-p))
              ;; Run in background to not block startup
              (run-with-idle-timer 
               1 nil 
               (lambda ()
                 ;; Only show messages if grammars are missing
                 (unless (funmacs--all-grammars-installed-p)
                   (message "[Funmacs] Checking tree-sitter grammars...")
                   (ignore-errors (funmacs-install-missing-grammars nil)))
                 ;; Silent check when all installed
                 (when (funmacs--all-grammars-installed-p)
                   (ignore-errors (funmacs-install-missing-grammars t)))))))))

(provide 'funmacs-treesitter)

;;; funmacs-treesitter.el ends here
