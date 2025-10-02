;;; funmacs-python.el -*- lexical-binding: t; -*-
(use-package python :ensure nil :mode (("\\.py\\'" . python-ts-mode)))
(add-hook 'python-ts-mode-hook #'eglot-ensure)
(provide 'funmacs-python)
