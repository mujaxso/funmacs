;;; funmacs-format.el -*- lexical-binding: t; -*-

;;; Commentary:
;; apheleia formatter

;;; Code:

(use-package apheleia
  :ensure t
  :defer t
  :diminish apheleia-mode
  :config
  ;; ============================================================
  ;; Systems Programming Languages
  ;; ============================================================
  
  ;; C/C++/Objective-C - clang-format (built-in)
  (setf (alist-get 'clang-format apheleia-formatters)
        '("clang-format" "--assume-filename" filepath))
  (setf (alist-get 'c-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c-ts-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-ts-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'objc-mode apheleia-mode-alist) 'clang-format)
  
  ;; Rust - rustfmt (built-in)
  (setf (alist-get 'rustfmt apheleia-formatters)
        '("rustfmt" "--quiet" "--emit" "stdout"))
  (setf (alist-get 'rust-mode apheleia-mode-alist) 'rustfmt)
  (setf (alist-get 'rust-ts-mode apheleia-mode-alist) 'rustfmt)
  (setf (alist-get 'rustic-mode apheleia-mode-alist) 'rustfmt)
  
  ;; Go - gofmt (built-in)
  (setf (alist-get 'gofmt apheleia-formatters)
        '("gofmt"))
  (setf (alist-get 'go-mode apheleia-mode-alist) 'gofmt)
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'gofmt)
  
  ;; Zig - zigfmt
  (setf (alist-get 'zigfmt apheleia-formatters)
        '("zig" "fmt" "--stdin"))
  (setf (alist-get 'zig-mode apheleia-mode-alist) 'zigfmt)
  (setf (alist-get 'zig-ts-mode apheleia-mode-alist) 'zigfmt)
  
  ;; ============================================================
  ;; Web Development Languages
  ;; ============================================================
  
  ;; ============================================================
  ;; Hybrid Approach: Biome (Fast) + Prettier (Comprehensive)
  ;; ============================================================
  
  ;; ──────────────────────────────────────────────────────────
  ;; Biome - Use for JS/TS/JSX/TSX/JSON (25x faster)
  ;; ──────────────────────────────────────────────────────────
  
  (setf (alist-get 'biome apheleia-formatters)
        '("biome" "format" "--stdin-file-path" filepath))
  
  ;; JavaScript/TypeScript with Biome
  (setf (alist-get 'javascript-mode apheleia-mode-alist) 'biome)
  (setf (alist-get 'js-mode apheleia-mode-alist) 'biome)
  (setf (alist-get 'js-ts-mode apheleia-mode-alist) 'biome)
  (setf (alist-get 'js2-mode apheleia-mode-alist) 'biome)
  (setf (alist-get 'js3-mode apheleia-mode-alist) 'biome)
  (setf (alist-get 'typescript-mode apheleia-mode-alist) 'biome)
  (setf (alist-get 'typescript-ts-mode apheleia-mode-alist) 'biome)
  (setf (alist-get 'tsx-ts-mode apheleia-mode-alist) 'biome)
  (setf (alist-get 'typescriptreact-mode apheleia-mode-alist) 'biome)
  (setf (alist-get 'rjsx-mode apheleia-mode-alist) 'biome)
  
  ;; JSON/JSONC with Biome
  (setf (alist-get 'json-mode apheleia-mode-alist) 'biome)
  (setf (alist-get 'json-ts-mode apheleia-mode-alist) 'biome)
  (setf (alist-get 'jsonc-mode apheleia-mode-alist) 'biome)
  
  ;; ──────────────────────────────────────────────────────────
  ;; Prettier - Use for everything else
  ;; ──────────────────────────────────────────────────────────
  
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath))
  
  ;; HTML/XML
  (setf (alist-get 'html-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'html-ts-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'mhtml-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'web-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'xml-mode apheleia-mode-alist) 'prettier)
  
  ;; CSS/SCSS/Sass/Less
  (setf (alist-get 'css-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'css-ts-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'scss-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'sass-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'less-css-mode apheleia-mode-alist) 'prettier)
  
  ;; YAML (not supported by Biome)
  (setf (alist-get 'yaml-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'yaml-ts-mode apheleia-mode-alist) 'prettier)
  
  ;; Markdown (not supported by Biome)
  (setf (alist-get 'markdown-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'gfm-mode apheleia-mode-alist) 'prettier)
  
  ;; GraphQL
  (setf (alist-get 'graphql-mode apheleia-mode-alist) 'prettier)
  
  ;; Vue/Svelte/Astro (not fully supported by Biome)
  (setf (alist-get 'vue-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'svelte-mode apheleia-mode-alist) 'prettier)
  (setf (alist-get 'astro-mode apheleia-mode-alist) 'prettier)
  
  ;; ──────────────────────────────────────────────────────────
  ;; Optional: Prettier with Tailwind plugin
  ;; ──────────────────────────────────────────────────────────
  
  (setf (alist-get 'prettier-tailwind apheleia-formatters)
        '("prettier" "--plugin" "prettier-plugin-tailwindcss" 
          "--stdin-filepath" filepath))
  
  ;; Uncomment to use Tailwind-aware formatting:
  ;; (setf (alist-get 'html-mode apheleia-mode-alist) 'prettier-tailwind)
  ;;
  
  ;; ============================================================
  ;; Python
  ;; ============================================================
  
  ;; Ruff - Modern, fast formatter (Black-compatible) with import sorting
  (setf (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "--silent" "--stdin-filename" filepath "-"))
  (setf (alist-get 'ruff-isort apheleia-formatters)
        '("ruff" "check" "--select" "I" "--fix" "--silent" "--stdin-filename" filepath "-"))
  
  ;; Use Ruff for both import sorting and formatting (chained)
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff))
  
  ;; ============================================================
  ;; Functional Languages
  ;; ============================================================
  
  ;; Haskell - brittany (built-in)
  (setf (alist-get 'brittany apheleia-formatters)
        '("brittany"))
  (setf (alist-get 'haskell-mode apheleia-mode-alist) 'brittany)
  
  ;; Elixir - mix-format (built-in)
  (setf (alist-get 'mix-format apheleia-formatters)
        '("mix" "format" "-"))
  (setf (alist-get 'elixir-mode apheleia-mode-alist) 'mix-format)
  (setf (alist-get 'elixir-ts-mode apheleia-mode-alist) 'mix-format)
  
  ;; OCaml - ocamlformat (built-in)
  (setf (alist-get 'ocamlformat apheleia-formatters)
        '("ocamlformat" "-" "--name" filepath "--enable-outside-detected-project"))
  (setf (alist-get 'caml-mode apheleia-mode-alist) 'ocamlformat)
  (setf (alist-get 'tuareg-mode apheleia-mode-alist) 'ocamlformat)
  
  ;; Lisp - lisp-indent (built-in)
  (setf (alist-get 'lisp-indent apheleia-formatters)
        'apheleia-indent-lisp-buffer)
  (setf (alist-get 'emacs-lisp-mode apheleia-mode-alist) 'lisp-indent)
  (setf (alist-get 'lisp-mode apheleia-mode-alist) 'lisp-indent)
  (setf (alist-get 'scheme-mode apheleia-mode-alist) 'lisp-indent)
  
  ;; ============================================================
  ;; JVM Languages
  ;; ============================================================
  
  ;; Java - google-java-format (built-in)
  (setf (alist-get 'google-java-format apheleia-formatters)
        '("google-java-format" "-"))
  (setf (alist-get 'java-mode apheleia-mode-alist) 'google-java-format)
  (setf (alist-get 'java-ts-mode apheleia-mode-alist) 'google-java-format)
  
  ;; ============================================================
  ;; Shell
  ;; ============================================================
  
  ;; Shell/Bash - shfmt (built-in)
  (setf (alist-get 'shfmt apheleia-formatters)
        '("shfmt" "-"))
  (setf (alist-get 'sh-mode apheleia-mode-alist) 'shfmt)
  (setf (alist-get 'bash-ts-mode apheleia-mode-alist) 'shfmt)
  
  ;; Fish - fish-indent (built-in)
  (setf (alist-get 'fish-indent apheleia-formatters)
        '("fish_indent"))
  (setf (alist-get 'fish-mode apheleia-mode-alist) 'fish-indent)
  
  ;; ============================================================
  ;; Ruby
  ;; ============================================================
  
  ;; Ruby - rubocop (built-in)
  (setf (alist-get 'rubocop apheleia-formatters)
        '("rubocop" "--stdin" filepath "--auto-correct" "--stderr" "--format" "quiet"))
  (setf (alist-get 'ruby-mode apheleia-mode-alist) 'rubocop)
  (setf (alist-get 'ruby-ts-mode apheleia-mode-alist) 'rubocop)
  
  ;; ============================================================
  ;; Configuration/Markup Languages
  ;; ============================================================
  
  ;; Nix - alejandra
  (setf (alist-get 'alejandra apheleia-formatters)
        '("alejandra" "--quiet" "-"))
  (setf (alist-get 'nix-mode apheleia-mode-alist) 'alejandra)
  (setf (alist-get 'nix-ts-mode apheleia-mode-alist) 'alejandra)
  
  ;; Terraform - terraform fmt (built-in)
  (setf (alist-get 'terraform apheleia-formatters)
        '("terraform" "fmt" "-"))
  (setf (alist-get 'terraform-mode apheleia-mode-alist) 'terraform)
  
  ;; TOML - prettier with plugin (built-in)
  ;; (setf (alist-get 'prettier-toml apheleia-formatters)
  ;;       '("prettier" "--plugin" "prettier-plugin-toml" "--stdin-filepath" filepath))
  ;; (setf (alist-get 'toml-mode apheleia-mode-alist) 'prettier-toml)
  ;; (setf (alist-get 'toml-ts-mode apheleia-mode-alist) 'prettier-toml)
  (setf (alist-get 'taplo apheleia-formatters)
	'("taplo" "fmt" "-"))
  (setf (alist-get 'toml-mode apheleia-mode-alist) 'taplo)
  (setf (alist-get 'toml-ts-mode apheleia-mode-alist) 'taplo)

  
  ;; LaTeX - latexindent (built-in)
  (setf (alist-get 'latexindent apheleia-formatters)
        '("latexindent" "--logfile=/dev/null"))
  (setf (alist-get 'latex-mode apheleia-mode-alist) 'latexindent)
  (setf (alist-get 'LaTeX-mode apheleia-mode-alist) 'latexindent)
  
  ;; Markdown - prettier (built-in)
  (setf (alist-get 'markdown-mode apheleia-mode-alist) 'prettier)
  
  ;; ============================================================
  ;; Hardware Description Languages
  ;; ============================================================
  
  ;; Verilog - iStyle (built-in)
  (setf (alist-get 'istyle-verilog apheleia-formatters)
        '("iStyle"))
  (setf (alist-get 'verilog-mode apheleia-mode-alist) 'istyle-verilog)
  
  ;; ============================================================
  ;; Other Languages
  ;; ============================================================
  
  ;; PHP - php-cs-fixer (built-in)
  (setf (alist-get 'php-cs-fixer apheleia-formatters)
        '("php-cs-fixer" "--quiet" "fix" filepath))
  (setf (alist-get 'php-mode apheleia-mode-alist) 'php-cs-fixer)
  
  ;; Dart - dartfmt (built-in)
  (setf (alist-get 'dartfmt apheleia-formatters)
        '("dartfmt"))
  (setf (alist-get 'dart-mode apheleia-mode-alist) 'dartfmt)
  
  ;; Lua - stylua (built-in)
  (setf (alist-get 'stylua apheleia-formatters)
        '("stylua" "-"))
  (setf (alist-get 'lua-mode apheleia-mode-alist) 'stylua)
  
  ;; V - vfmt (built-in)
  (setf (alist-get 'vfmt apheleia-formatters)
        '("v" "fmt" "-"))
  (setf (alist-get 'v-mode apheleia-mode-alist) 'vfmt)
  
  ;; ============================================================
  ;; Configuration Options
  ;; ============================================================
  
  :custom
  ;; Respect Emacs indentation settings
  (apheleia-formatters-respect-indent-level t)
  ;; Hide log buffers from buffer list
  (apheleia-hide-log-buffers t)
  ;; Only log errors
  (apheleia-log-only-errors t)
  ;; Remote file formatting
  (apheleia-remote-algorithm 'local)
  
  ;; Enable globally
  (apheleia-global-mode +1))


(provide 'funmacs-format)

;;; funmacs-format.el ends here
