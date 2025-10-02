;;; funmacs-embark.el -*- lexical-binding: t; -*-

;;; Commentary:
;; embark

;;; code

(use-package embark
  :bind (("C-." . embark-act) ("C-;" . embark-dwim)))


(use-package embark-consult
  :after (embark consult)
  :ensure t)

(provide 'funmacs-embark)

;;; funmacs-embark.el ends here
