;;; funmacs-dashboard.el -*- lexical-binding: t; -*-

;;; Commentary:
;; An extensible emacs startup screen showing you what’s most important.

;;; code

(use-package dashboard
  :ensure t
  :demand t
  :config
  (dashboard-setup-startup-hook)
  ;; disable white spaces
  (add-hook 'dashboard-mode-hook (lambda () (setq show-trailing-whitespace nil)))
  ;; show Dashboard in frames created with emacsclient -c
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  ;; Set the title
  ;; (setq dashboard-banner-logo-title "Welcome to Funmacs")
  (setq dashboard-banner-logo-title
        (message "Funmacs ready in %s with %d garbage collections."
                 (format "%.2f seconds"
                         (float-time
                          (time-subtract after-init-time before-init-time)))
                 gcs-done))
  ;; Set the banner
  (setq dashboard-startup-banner "~/.config/emacs/assets/txt/logo.txt")
  ;; Value can be
  ;; - nil to display no banner
  ;; - 'official which displays the official emacs logo
  ;; - 'logo which displays an alternative emacs logo
  ;; - 1, 2 or 3 which displays one of the text banners
  ;; - "path/to/your/image.gif", "path/to/your/image.png"
  ;; or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer
  ;; - a cons of '("path/to/your/image.png" . "path/to/your/text.txt")
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  ;; choose the default project managment backend
  (setq dashboard-projects-backend 'project-el)
  ;; add icons to the widget headings and their items
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ;; display icons on both GUI and terminal
  (setq dashboard-display-icons-p t)
  ;; use `nerd-icons' package (disabled for now to avoid dependency issues)
  (setq dashboard-icon-type nil)
  ;; To customize which widgets are displayed
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5)))
  ;; set naviagtion banner.
  (setq dashboard-set-navigator t)
  ;; navigation buttons (without icons for now)
  (setq dashboard-navigator-buttons
        `((("🏠" "Homepage" "Browse Homepage" 
            (lambda (&rest _) (browse-url "https://funmacs.mujaxso.com")))
           ("🐙" "GitHub" "Browse GitHub" 
            (lambda (&rest _) (browse-url "https://github.com/mujaxso/funmacs")))
           ("🐦" "Twitter" "Browse Twitter" 
            (lambda (&rest _) (browse-url "https://www.twitter.com/mujaxso")))
           ("💼" "LinkedIn" "Browse LinkedIn" 
            (lambda (&rest _) (browse-url "https://www.linkedin.com/in/mujaxso"))))))
  )

(provide 'funmacs-dashboard)

;;; funmacs-dashboard.el ends here.
