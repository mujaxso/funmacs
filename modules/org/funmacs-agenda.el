;;; funmacs-agenda.el --- Complete Org Agenda Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive Org agenda with fast views, tidy prefixes, and helpful reviews.

;;; Code:
(require 'org)
(require 'org-agenda)

;; Files: recurse .org, exclude dotfiles/backups; keep small for speed
(setq org-agenda-files
      (when (file-directory-p org-directory)
        (directory-files-recursively org-directory "\\`[^.].*\\.org\\'")))

;; Core speed ups
(setq org-agenda-inhibit-startup t)          ;; do not honor per-file #+STARTUP on first visit
(setq org-agenda-use-tag-inheritance nil)    ;; no tag inheritance cost in listings
(setq org-agenda-dim-blocked-tasks nil)      ;; avoid parent-chain checks
(setq org-agenda-ignore-properties '(stats)) ;; skip [%]/[n/m] cookie updates

;; Skip done everywhere
(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-timestamp-if-done t)

;; Sticky agendas: reuse buffer for dashboard
(setq org-agenda-sticky t)

;; Time grid kept light
(setq org-agenda-current-time-string "⭠ now ───────────")
(setq org-agenda-time-grid
      '((daily today require-timed remove-match)
        (800 1000 1200 1400 1600 1800 2000)
        "....." "──────────────"))

;; Prefix formats: keep simple in non-agenda views
(setq org-agenda-prefix-format
      '((agenda . " %i %-12:c%?-12t% s")
        (todo   . " %i %-12:c")
        (tags   . " %i %-12:c")
        (search . " %i %-12:c")))

;; Sorting: cheap, predictable
(setq org-agenda-sorting-strategy
      '((agenda time-up priority-down category-keep)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep)))

;; Lighter custom commands
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ;; Single calendar + three cheap lists
         ((agenda "" ((org-agenda-span 'day)
                      (org-agenda-start-on-weekday nil)
                      (org-deadline-warning-days 7)
                      (org-agenda-overriding-header "Today\n")))
          (tags-todo "+PRIORITY=\"A\""
                     ((org-agenda-overriding-header "\nHigh Priority\n")
                      ;; No redundant calendar scan here
                      (org-agenda-skip-function
                       '(org-agenda-skip-entry-if 'scheduled 'deadline))))
          (todo "IN-PROGRESS"
                ((org-agenda-overriding-header "\nIn Progress\n")))
          (todo "WAITING"
                ((org-agenda-overriding-header "\nWaiting\n")))))

        ("w" "Weekly"
         ((agenda "" ((org-agenda-span 7)
                      (org-agenda-start-on-weekday 1)
                      (org-agenda-overriding-header
                       (concat "Week " (format-time-string "%V") "\n"))))))

        ("n" "Next"
         ((todo "TODO"
                ((org-agenda-overriding-header "Next Actions\n")
                 (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'scheduled 'deadline))))))

        ("s" "Stuck Projects"
         ((tags "PROJECT"
                ((org-agenda-overriding-header "Stuck Projects\n")
                 ;; Cheap check: skip if any child hints action keywords
                 (org-agenda-skip-function
                  '(org-agenda-skip-if nil '(regexp ":NEXT:\\|TODO\\|IN-PROGRESS")))))))
        ))

;; Built-in stuck detector tuned to your keywords
(setq org-stuck-projects
      '("+PROJECT/-DONE" ("NEXT" "IN-PROGRESS" "WAITING") nil ""))

;; Cheap logging view defaults
(setq org-agenda-start-with-log-mode nil)
(setq org-agenda-log-mode-items '(closed clock state))
(setq org-agenda-clockreport-parameter-plist
      '(:link t :maxlevel 3 :fileskip0 t :compact t :narrow 60))

;; QoL: autosave on entering agenda (cheap)
(add-hook 'org-agenda-mode-hook #'org-save-all-org-buffers)
(setq org-agenda-show-future-repeats 'next)

(provide 'funmacs-agenda)
;;; funmacs-agenda.el ends here
