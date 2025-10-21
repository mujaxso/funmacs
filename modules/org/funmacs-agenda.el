;;; funmacs-agenda.el --- Complete Org Agenda Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive Org agenda with fast views, tidy prefixes, and helpful reviews.

;;; Code:

(require 'org)
(require 'org-agenda)

;; -------------------------------
;; Basic settings
;; -------------------------------
;; Use explicit files or a function for speed; directory scan also works.
(setq org-agenda-files (list (expand-file-name "~/.org") (expand-file-name "~/org")))
;; Alternatively:
;; (setq org-agenda-files (directory-files-recursively org-directory "\\.org\\'"))

(setq org-agenda-start-on-weekday 1)   ;; Monday
(setq org-agenda-span 7)               ;; 7-day block by default
(setq org-agenda-start-day nil)        ;; start from today
(setq org-deadline-warning-days 14)

;; -------------------------------
;; Performance
;; -------------------------------
(setq org-agenda-inhibit-startup t)
(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-use-tag-inheritance nil)
(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-timestamp-if-done t)

;; -------------------------------
;; Logging / clocking
;; -------------------------------
(setq org-agenda-start-with-log-mode nil)
(setq org-agenda-log-mode-items '(closed clock state))
(setq org-agenda-clockreport-parameter-plist
      '(:link t :maxlevel 3 :fileskip0 t :compact t :narrow 60))

;; -------------------------------
;; Display tuning
;; -------------------------------
(setq org-agenda-current-time-string "ᐊ─ now ─────────────")
(setq org-agenda-time-grid
      '((daily today require-timed remove-match)
        (800 1000 1200 1400 1600 1800 2000)
        "......" "────────────────"))
(setq org-agenda-compact-blocks t)

;; Keep %s only where it works (agenda); use expressions elsewhere if needed.
(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " %i %-12:c")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))

(setq org-agenda-sorting-strategy
      '((agenda time-up priority-down category-keep)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep)))

;; -------------------------------
;; Custom commands
;; -------------------------------
(setq org-agenda-custom-commands
      '(;; Daily focus
        ("d" "Daily Agenda"
         ((agenda ""
                  ((org-agenda-span 'day)
                   (org-agenda-overriding-header "Today's Schedule\n")
                   (org-deadline-warning-days 7)))
          (tags-todo "+PRIORITY=\"A\""
                     ((org-agenda-overriding-header "\nHigh Priority Tasks\n")))
          (tags-todo "-PRIORITY=\"A\""
                     ((org-agenda-overriding-header "\nOther Tasks\n")
                      (org-agenda-skip-function
                       '(org-agenda-skip-entry-if 'scheduled 'deadline))))))

        ;; Weekly overview
        ("w" "Weekly Overview"
         ((agenda ""
                  ((org-agenda-span 7)
                   (org-agenda-start-on-weekday 1)
                   (org-agenda-overriding-header
                    (concat "Week " (format-time-string "%V") " Overview\n"))))))

        ;; Planning
        ("p" "Planning"
         ((tags-todo "+@planning"
                     ((org-agenda-overriding-header "Planning Tasks\n")))
          (tags-todo "-{.*}"
                     ((org-agenda-overriding-header "\nUntagged Tasks\n")))
          (todo "TODO|IN-PROGRESS"
                ((org-agenda-overriding-header "\nAll Active Tasks\n")))))

        ;; Weekly review: done vs unfinished
        ("r" "Weekly Review"
         ((agenda ""
                  ((org-agenda-overriding-header "Completed Tasks\n")
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'nottodo 'done))
                   (org-agenda-span 'week)))
          (agenda ""
                  ((org-agenda-overriding-header "\nUnfinished Scheduled Tasks\n")
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-span 'week)))))

        ;; Next actions and statuses
        ("n" "Next Actions"
         ((todo "TODO"
                ((org-agenda-overriding-header "Next Actions\n")
                 (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'scheduled 'deadline))))
          (todo "IN-PROGRESS"
                ((org-agenda-overriding-header "\nIn Progress\n")))
          (todo "WAITING"
                ((org-agenda-overriding-header "\nWaiting For\n")))))

        ;; Contexts
        ("@" "Contexts"
         ((tags-todo "@work"   ((org-agenda-overriding-header "Work Tasks\n")))
          (tags-todo "@home"   ((org-agenda-overriding-header "\nHome Tasks\n")))
          (tags-todo "@errands"((org-agenda-overriding-header "\nErrands\n")))))

        ;; Projects
        ("P" "Projects"
         ((tags "PROJECT"
                ((org-agenda-overriding-header "Active Projects\n")
                 (org-tags-match-list-sublevels 'indented)))))

        ;; Stuck projects (simple matcher version)
        ("s" "Stuck Projects"
         ((tags "PROJECT"
                ((org-agenda-overriding-header "Stuck Projects\n")
                 (org-agenda-skip-function
                  '(org-agenda-skip-if nil '(regexp ":NEXT:\\|TODO\\|IN-PROGRESS")))))))
        ))

;; -------------------------------
;; Optional: use built-in stuck detector
;; -------------------------------
;; A stuck project has no next actions; tune to your keywords/tags.
(setq org-stuck-projects
      '("+PROJECT/-DONE"
        ("NEXT" "IN-PROGRESS" "WAITING")
        nil
        ""))  ;; no regexp for nonproject

;; -------------------------------
;; Keybindings
;; -------------------------------
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c o d") (lambda () (interactive) (org-agenda nil "d")))
(global-set-key (kbd "C-c o w") (lambda () (interactive) (org-agenda nil "w")))

;; -------------------------------
;; Hooks / quality-of-life
;; -------------------------------
(add-hook 'org-agenda-mode-hook #'org-save-all-org-buffers)
(setq org-agenda-show-future-repeats 'next)

;; Optional: export appointments to Emacs appt system from agenda files.
;; (org-agenda-to-appt)
;; (add-hook 'org-finalize-agenda-hook #'org-agenda-to-appt)

(provide 'funmacs-agenda)
;;; funmacs-agenda.el ends here
