;;; funmacs-typescript.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern web development configuration for TypeScript, React, Next.js, and MDX
;; Requires: typescript-language-server, biome, eslint, prettier (install globally or per-project)
;; npm install -g typescript-language-server typescript

;;; Code:

;; Use tree-sitter modes for better performance and syntax highlighting
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mts\\'" . typescript-ts-mode))

;; MDX support - treat as markdown with JSX capabilities
(add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode))

;; Configure eglot server programs
(with-eval-after-load 'eglot
  ;; Ensure typescript-language-server is used for all TypeScript/React modes
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode)
                 . ("typescript-language-server" "--stdio")))
  
  ;; Optional: Configure workspace settings for better React/Next.js support
  (setq-default eglot-workspace-configuration
                '(:typescript
                  (:tsserver
                   (:preferences
                    (:includeInlayParameterNameHints "all"
						     :includeInlayParameterNameHintsWhenArgumentMatchesName t
						     :includeInlayFunctionParameterTypeHints t
						     :includeInlayVariableTypeHints t
						     :includeInlayPropertyDeclarationTypeHints t
						     :includeInlayFunctionLikeReturnTypeHints t
						     :includeInlayEnumMemberValueHints t))))))

;; Helper function to use local node_modules binaries
(defun funmacs/add-node-modules-path ()
  "Add local node_modules/.bin to exec-path for project-specific tools."
  (when-let* ((root (locate-dominating-file default-directory "node_modules"))
              (bin-path (expand-file-name "node_modules/.bin" root)))
    (when (file-directory-p bin-path)
      (add-to-list 'exec-path bin-path)
      (setenv "PATH" (concat bin-path ":" (getenv "PATH"))))))

;; Enhanced TypeScript mode hook
(defun funmacs/typescript-mode-setup ()
  "Configure TypeScript/TSX development environment."
  (funmacs/add-node-modules-path)
  (eglot-ensure)
  (setq-local tab-width 2)
  (setq-local js-indent-level 2)
  (electric-pair-local-mode 1)
  (subword-mode 1))

;; Apply configuration to all TypeScript/React modes
(add-hook 'typescript-ts-mode-hook #'funmacs/typescript-mode-setup)
(add-hook 'tsx-ts-mode-hook #'funmacs/typescript-mode-setup)

;; JavaScript modes for Next.js configuration files
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-hook 'js-ts-mode-hook #'funmacs/typescript-mode-setup)

;; JSON mode for package.json, tsconfig.json, etc.
(add-hook 'json-ts-mode-hook #'eglot-ensure)

;; Flymake keybindings for error navigation
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") #'flymake-goto-prev-error))

;; Optional: Enable format-on-save with prettier (if available)
(defun funmacs/format-on-save ()
  "Format buffer on save if eglot is active."
  (when (and (bound-and-true-p eglot--managed-mode)
             (eglot-current-server))
    (eglot-format-buffer)))

;; Uncomment to enable auto-formatting on save
;; (add-hook 'before-save-hook #'funmacs/format-on-save nil t)

;; CSS/SCSS support for styling Next.js apps
(add-hook 'css-ts-mode-hook #'eglot-ensure)
(add-hook 'scss-mode-hook #'eglot-ensure)

;; Markdown configuration for MDX
(with-eval-after-load 'markdown-mode
  ;; Enable syntax highlighting for code blocks
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-math t))

(provide 'funmacs-typescript)

;;; funmacs-typescript.el ends here
