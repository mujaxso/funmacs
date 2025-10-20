;;; funmacs-agenda.el --- Complete Org Agenda Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive Org agenda setup with custom views and optimizations

;;; Code:

;; =============================================================================
;; BASIC AGENDA SETTINGS
;; =============================================================================

;; Agenda files location
(setq org-agenda-files '("~/org/"))

;; Start week on Monday (0 = Sunday, 1 = Monday)
(setq org-agenda-start-on-weekday 1)

;; Show 7 days by default
(setq org-agenda-span 7)

;; Start agenda from today
(setq org-agenda-start-day nil)

;; Deadline warning days
(setq org-deadline-warning-days 14)

;; =============================================================================
;; PERFORMANCE OPTIMIZATIONS
;; =============================================================================

;; Prevent agenda from loading org files at startup
(setq org-agenda-inhibit-startup t)

;; Disable expensive dimming
(setq org-agenda-dim-blocked-tasks nil)

;; Disable tag inheritance in agenda
(setq org-agenda-use-tag-inheritance nil)

;; Skip completed items
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-timestamp-if-done t)

;; =============================================================================
;; LOGGING AND TIME TRACKING
;; =============================================================================

;; Start agenda with log mode enabled
(setq org-agenda-start-with-log-mode nil)

;; Show clocked items
(setq org-agenda-log-mode-items '(closed clock state))

;; Clock report parameters
(setq org-agenda-clockreport-parameter-plist
      '(:link t :maxlevel 3 :fileskip0 t :compact t :narrow 60))

;; =============================================================================
;; DISPLAY SETTINGS
;; =============================================================================

;; Show current time in agenda
(setq org-agenda-current-time-string "◀── now ──────────────")

;; Time grid settings
(setq org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        "......" "────────────────"))

;; Compact block agenda display
(setq org-agenda-compact-blocks t)

;; Agenda prefix format
(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))

;; =============================================================================
;; SORTING STRATEGIES
;; =============================================================================

(setq org-agenda-sorting-strategy
      '((agenda time-up priority-down category-keep)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep)))

;; =============================================================================
;; CUSTOM AGENDA VIEWS
;; =============================================================================

(setq org-agenda-custom-commands
      '(
        ;; Daily agenda view
        ("d" "Daily Agenda"
         ((agenda "" ((org-agenda-span 'day)
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
         ((agenda "" ((org-agenda-span 7)
                      (org-agenda-start-on-weekday 1)
                      (org-agenda-overriding-header
                       (concat "Week " (format-time-string "%V") " Overview\n"))))))

        ;; Planning view
        ("p" "Planning"
         ((tags-todo "+@planning"
                     ((org-agenda-overriding-header "Planning Tasks\n")))
          (tags-todo "-{.*}"
                     ((org-agenda-overriding-header "\nUntagged Tasks\n")))
          (todo "TODO|IN-PROGRESS"
                ((org-agenda-overriding-header "\nAll Active Tasks\n")))))

        ;; Weekly review
        ("r" "Weekly Review"
         ((agenda "" ((org-agenda-overriding-header "Completed Tasks\n")
                      (org-agenda-skip-function
                       '(org-agenda-skip-entry-if 'nottodo 'done))
                      (org-agenda-span 'week)))
          (agenda "" ((org-agenda-overriding-header "\nUnfinished Scheduled Tasks\n")
                      (org-agenda-skip-function
                       '(org-agenda-skip-entry-if 'todo 'done))
                      (org-agenda-span 'week)))))

        ;; Next actions
        ("n" "Next Actions"
         ((todo "TODO"
                ((org-agenda-overriding-header "Next Actions\n")
                 (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'scheduled 'deadline))))
          (todo "IN-PROGRESS"
                ((org-agenda-overriding-header "\nIn Progress\n")))
          (todo "WAITING"
                ((org-agenda-overriding-header "\nWaiting For\n")))))

        ;; Context-based views
        ("@" "Contexts"
         ((tags-todo "@work"
                     ((org-agenda-overriding-header "Work Tasks\n")))
          (tags-todo "@home"
                     ((org-agenda-overriding-header "\nHome Tasks\n")))
          (tags-todo "@errands"
                     ((org-agenda-overriding-header "\nErrands\n")))))

        ;; Project view
        ("P" "Projects"
         ((tags "PROJECT"
                ((org-agenda-overriding-header "Active Projects\n")
                 (org-tags-match-list-sublevels 'indented)))))

        ;; Search stuck projects
        ("s" "Stuck Projects"
         ((tags "PROJECT"
                ((org-agenda-overriding-header "Stuck Projects\n")
                 (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'todo '("TODO" "IN-PROGRESS")))))))
        ))

;; =============================================================================
;; KEYBINDINGS
;; =============================================================================

;; Global agenda keybinding
(global-set-key (kbd "C-c a") 'org-agenda)

;; Quick access to custom views
(global-set-key (kbd "C-c o d") (lambda () (interactive) (org-agenda nil "d")))
(global-set-key (kbd "C-c o w") (lambda () (interactive) (org-agenda nil "w")))

;; =============================================================================
;; AGENDA HOOKS
;; =============================================================================

;; Auto-save org buffers before agenda
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (org-save-all-org-buffers)))

;; Remove empty blocks from agenda
(setq org-agenda-show-future-repeats 'next)

(provide 'funmacs-agenda)
;;; org-agenda-config.el ends here
