;;; funmacs-embark.el -*- lexical-binding: t; -*-
(use-package embark :bind (("C-." . embark-act) ("C-;" . embark-dwim)))
(use-package embark-consult :after (embark consult))
(provide 'funmacs-embark)
