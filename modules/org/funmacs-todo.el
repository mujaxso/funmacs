;;; funmacs-todo.el --- Org TODO workflow -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive TODO/agenda workflow with effort, logging, and parent auto-state.

;;; Code:

(require 'org) ;; ensure variables are defined before customization

;; -------------------------------
;; TODO keywords and faces
;; -------------------------------
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "WAITING(w@/!)" "HOLD(h)"
                  "|" "DONE(d!)" "CANCELLED(c@)")
        (sequence "PROJECT(p)" "|" "COMPLETED(C)" "ARCHIVED(A@)")
        (sequence "IDEA(I)" "DRAFT(D)" "|" "PUBLISHED(P)")
        (sequence "BUG(b)" "FIXED(f)" "|" "VERIFIED(v)")))  ;; [Manual: TODO keywords]

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#ff6c6b" :weight bold))
        ("NEXT" . (:foreground "#ff79c6" :weight bold))
        ("IN-PROGRESS" . (:foreground "#f1fa8c" :weight bold))
        ("WAITING" . (:foreground "#ffb86c" :weight bold))
        ("HOLD" . (:foreground "#bd93f9" :weight bold))
        ("DONE" . (:foreground "#50fa7b" :weight bold))
        ("CANCELLED" . (:foreground "#6272a4" :weight bold :strike-through t))
        ("PROJECT" . (:foreground "#8be9fd" :weight bold))
        ("COMPLETED" . (:foreground "#50fa7b" :weight bold))
        ("ARCHIVED" . (:foreground "#6272a4"))
        ("IDEA" . (:foreground "#bd93f9"))
        ("DRAFT" . (:foreground "#f1fa8c"))
        ("PUBLISHED" . (:foreground "#50fa7b"))
        ("BUG" . (:foreground "#ff5555" :weight bold))
        ("FIXED" . (:foreground "#ffb86c"))
        ("VERIFIED" . (:foreground "#50fa7b"))))  ;; [Manual: Faces and TODOs]

;; -------------------------------
;; Logging
;; -------------------------------
(setq org-log-done 'time
      org-log-reschedule 'time
      org-log-redeadline 'time
      org-log-into-drawer "LOGBOOK"          ;; string drawer name per manual
      org-log-state-notes-into-drawer "LOGBOOK"
      org-log-note-headings
      '((done . "CLOSING NOTE %t")
        (state . "State %-12s from %-12S %t")
        (note . "Note taken on %t")
        (reschedule . "Rescheduled from %S on %t")
        (delschedule . "Not scheduled, was %S on %t")
        (redeadline . "New deadline from %S on %t")
        (deldeadline . "Removed deadline, was %S on %t")
        (refile . "Refiled on %t")
        (clock-out . "")))  ;; [Manual: Tracking TODO state changes][web:20][web:14]

;; -------------------------------
;; Priorities
;; -------------------------------
(setq org-highest-priority ?A
      org-lowest-priority  ?E
      org-default-priority ?C
      org-priority-faces
      '((?A . (:foreground "#ff5555" :weight bold))
        (?B . (:foreground "#ffb86c" :weight bold))
        (?C . (:foreground "#f1fa8c"))
        (?D . (:foreground "#8be9fd"))
        (?E . (:foreground "#6272a4"))))  ;; [Manual: Priorities]

;; -------------------------------
;; Tags
;; -------------------------------
(setq org-tag-alist
      '((:startgroup . nil)
        ("@work" . ?w) ("@home" . ?h) ("@errands" . ?e)
        (:endgroup . nil)
        (:startgroup . nil)
        ("@office" . ?o) ("@online" . ?O) ("@phone" . ?p) ("@computer" . ?c)
        (:endgroup . nil)
        ("coding" . ?C) ("writing" . ?W) ("reading" . ?R) ("meeting" . ?M) ("planning" . ?P)
        ("urgent" . ?u) ("important" . ?i) ("someday" . ?s)
        ("project" . ?j) ("nixos" . ?n) ("zig" . ?z)))
(setq org-tag-persistent-alist org-tag-alist)  ;; [Manual: Tags]

;; -------------------------------
;; Effort and columns
;; -------------------------------
(setq org-global-properties
      '(("Effort_ALL" . "0:15 0:30 1:00 2:00 3:00 4:00 6:00 8:00")))
(setq org-agenda-columns-add-appointments-to-effort-sum t) ;; include timed appts
(setq org-columns-default-format
      "%50ITEM(Task) %10TODO %3PRIORITY %10Effort(Effort){:} %10CLOCKSUM")  ;; [Manual: Effort Estimates]
;; Tip: Use agenda column view and effort filters for planning (/ and e) [web:8][web:12]

;; -------------------------------
;; Dependencies and repeats
;; -------------------------------
(setq org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-agenda-dim-blocked-tasks 'invisible
      org-agenda-show-future-repeats t
      org-log-repeat 'time)  ;; [Manual: Breaking down tasks and dependencies][web:19]

;; -------------------------------
;; Interactive helpers
;; -------------------------------
;;;###autoload
(defun funmacs/org--ensure ()
  "Ensure Org context and buffer."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer")))

;;;###autoload
(defun funmacs/org-todo-next () (interactive) (funmacs/org--ensure) (org-todo "NEXT"))  ;; [Manual: TODO state change][web:20]
;;;###autoload
(defun funmacs/org-todo-in-progress () (interactive) (funmacs/org--ensure) (org-todo "IN-PROGRESS"))  ;; [Manual: TODO state change][web:20]
;;;###autoload
(defun funmacs/org-todo-waiting () (interactive) (funmacs/org--ensure) (org-todo "WAITING"))  ;; [Manual: TODO state change][web:20]
;;;###autoload
(defun funmacs/org-todo-done () (interactive) (funmacs/org--ensure) (org-todo "DONE"))  ;; [Manual: TODO state change][web:20]
;;;###autoload
(defun funmacs/org-todo-cancel () (interactive) (funmacs/org--ensure) (org-todo "CANCELLED"))  ;; [Manual: TODO state change][web:20]

;; When clocking in, mark IN-PROGRESS
(defun funmacs/org-clock-in-set-next ()
  "Set task to IN-PROGRESS when clocking in."
  (when (and (org-entry-is-todo-p)
             (not (equal (org-get-todo-state) "IN-PROGRESS")))
    (org-todo "IN-PROGRESS")))  ;; [Manual: Tracking TODO state changes][web:20]
(add-hook 'org-clock-in-hook #'funmacs/org-clock-in-set-next)  ;; [Manual hooks][web:16]

;; Auto-update parent state based on children; requires a statistics cookie
(defun funmacs/org-update-parent-todo (n-done n-not-done)
  "Update parent TODO based on N-DONE and N-NOT-DONE."
  (let (org-log-done org-log-states)
    (org-todo (cond
               ((= n-not-done 0) "DONE")
               ((= n-done 0) "TODO")
               (t "IN-PROGRESS")))))  ;; pattern per manual + extended middle state
(add-hook 'org-after-todo-statistics-hook #'funmacs/org-update-parent-todo)  ;; [Manual: org-after-todo-statistics-hook][web:16][web:13]

;; -------------------------------
;; Keybindings
;; -------------------------------
(with-eval-after-load 'org
  (let ((map org-mode-map))
    (define-key map (kbd "C-c t n") #'funmacs/org-todo-next)
    (define-key map (kbd "C-c t i") #'funmacs/org-todo-in-progress)
    (define-key map (kbd "C-c t w") #'funmacs/org-todo-waiting)
    (define-key map (kbd "C-c t d") #'funmacs/org-todo-done)
    (define-key map (kbd "C-c t c") #'funmacs/org-todo-cancel)
    (define-key map (kbd "C-c p")   #'org-priority)
    (define-key map (kbd "C-c e")   #'org-set-effort)))  ;; [Manual: Effort & priority commands][web:8]

(provide 'funmacs-todo)
;;; funmacs-todo.el ends here
