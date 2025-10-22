;;; funmacs-aidermacs.el --- Aidermacs integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Aider is an AI pair programming tool. This module integrates Aider into Emacs
;; via aidermacs. It uses environment variables for API keys and opens the aider
;; buffer on the right side of the frame by default.
;;
;; Supported providers:
;; - OpenAI:        OPENAI_API_KEY
;; - Anthropic:     ANTHROPIC_API_KEY
;; - Gemini:        GOOGLE_API_KEY
;; - Mistral:       MISTRAL_API_KEY
;; - TogetherAI:    TOGETHER_API_KEY
;; - OpenRouter:    OPENROUTER_API_KEY
;; - DeepSeek:      DEEPSEEK_API_KEY

;;; Code:

;;; Code:

(use-package aidermacs
  :ensure t
  :commands (aidermacs-chat)
  :bind (("C-c i" . aidermacs-transient-menu))
  :config
  ;; Set the correct executable name for NixOS
  (setq aidermacs-program "aider")  ; NixOS package creates 'aider' command
  
  ;; Default provider DeepSeek
  (setq aidermacs-default-provider 'deepseek)

  ;; Default model (OpenRouter)
  (setq aidermacs-default-model "openrouter/qwen/qwen3-235b-a22b:free"
        aidermacs-auto-commits nil
        aidermacs-use-git t)
  
  ;; API keys from environment
  (setq aidermacs-api-keys
        `((gemini    . ,(getenv "GEMINI_API_KEY"))
          (deepseek  . ,(getenv "DEEPSEEK_API_KEY"))
          (openai    . ,(getenv "OPENAI_API_KEY"))
          (anthropic . ,(getenv "ANTHROPIC_API_KEY"))
          (openrouter . ,(getenv "OPENROUTER_API_KEY"))))

  ;; Open in side window on right
  (setq display-buffer-alist
        `(("*aider*"
           (display-buffer-in-side-window)
           (side . right)
           (slot . 0)
           (window-width . 0.4))))

  ;; Disable wrapping in the Aider buffer
  (add-hook 'aidermacs-mode-hook
            (lambda ()
              (visual-line-mode -1)
              (setq truncate-lines t)
              (setq word-wrap nil))))

(provide 'funmacs-aidermacs)

;;; funmacs-aidermacs.el ends here
