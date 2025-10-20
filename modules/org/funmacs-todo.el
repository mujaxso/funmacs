;;; funmacs-todo.el --- Complete Org TODO Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive TODO workflow setup for Org-mode

;;; Code:

;; =============================================================================
;; TODO KEYWORDS AND WORKFLOW
;; =============================================================================

;; Define TODO workflow states
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "IN-PROGRESS(i)" "WAITING(w@/!)" "HOLD(h)" 
                  "|" "DONE(d!)" "CANCELLED(c@)")
        (sequence "PROJECT(p)" "|" "COMPLETED(C)" "ARCHIVED(A@)")
        (sequence "IDEA(I)" "DRAFT(D)" "|" "PUBLISHED(P)")
        (sequence "BUG(b)" "FIXED(f)" "|" "VERIFIED(v)")))

;; Explanation of notation:
;; (t) = fast selection key
;; @   = add note with timestamp when entering this state
;; !   = add timestamp when entering this state
;; @/! = add note when entering, timestamp when leaving
;; |   = separates active states from done states

;; Alternative simple workflow:
;; (setq org-todo-keywords
;;       '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" 
;;                   "|" "DONE(d)" "CANCELLED(c)")))

;; =============================================================================
;; TODO KEYWORD FACES (COLORS)
;; =============================================================================

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
        ("VERIFIED" . (:foreground "#50fa7b"))))

;; =============================================================================
;; LOGGING CONFIGURATION
;; =============================================================================

;; Log time when marking DONE
(setq org-log-done 'time)

;; Log when tasks are rescheduled or deadlines changed
(setq org-log-reschedule 'time)
(setq org-log-redeadline 'time)

;; Log state changes into a drawer
(setq org-log-into-drawer t)

;; Drawer name for logging
(setq org-log-state-notes-into-drawer "LOGBOOK")

;; Note templates for state changes
(setq org-log-note-headings
      '((done . "CLOSING NOTE %t")
        (state . "State %-12s from %-12S %t")
        (note . "Note taken on %t")
        (reschedule . "Rescheduled from %S on %t")
        (delschedule . "Not scheduled, was %S on %t")
        (redeadline . "New deadline from %S on %t")
        (deldeadline . "Removed deadline, was %S on %t")
        (refile . "Refiled on %t")
        (clock-out . "")))

;; =============================================================================
;; TODO PRIORITIES
;; =============================================================================

;; Priority range: A (highest) to E (lowest)
(setq org-highest-priority ?A)
(setq org-lowest-priority ?E)
(setq org-default-priority ?C)

;; Priority faces
(setq org-priority-faces
      '((?A . (:foreground "#ff5555" :weight bold))
        (?B . (:foreground "#ffb86c" :weight bold))
        (?C . (:foreground "#f1fa8c"))
        (?D . (:foreground "#8be9fd"))
        (?E . (:foreground "#6272a4"))))

;; =============================================================================
;; TAGS CONFIGURATION
;; =============================================================================

;; Tag list with fast selection keys
(setq org-tag-alist
      '(;; Context tags
        (:startgroup . nil)
        ("@work" . ?w)
        ("@home" . ?h)
        ("@errands" . ?e)
        (:endgroup . nil)
        
        ;; Location tags
        (:startgroup . nil)
        ("@office" . ?o)
        ("@online" . ?O)
        ("@phone" . ?p)
        ("@computer" . ?c)
        (:endgroup . nil)
        
        ;; Activity tags
        ("coding" . ?C)
        ("writing" . ?W)
        ("reading" . ?R)
        ("meeting" . ?M)
        ("planning" . ?P)
        
        ;; Priority contexts
        ("urgent" . ?u)
        ("important" . ?i)
        ("someday" . ?s)
        
        ;; Project tags
        ("project" . ?j)
        ("nixos" . ?n)
        ("zig" . ?z)))

;; Persistent tags
(setq org-tag-persistent-alist org-tag-alist)

;; =============================================================================
;; EFFORT ESTIMATES
;; =============================================================================

;; Effort estimate values for quick selection
(setq org-global-properties
      '(("Effort_ALL" . "0:15 0:30 1:00 2:00 3:00 4:00 6:00 8:00")))

;; Show effort estimates in agenda
(setq org-agenda-columns-add-appointments-to-effort-sum t)

;; Column view format
(setq org-columns-default-format
      "%50ITEM(Task) %10TODO %3PRIORITY %10Effort(Effort){:} %10CLOCKSUM")

;; =============================================================================
;; TODO DEPENDENCIES
;; =============================================================================

;; Enforce TODO dependencies
(setq org-enforce-todo-dependencies t)

;; Block entries with uncompleted children
(setq org-enforce-todo-checkbox-dependencies t)

;; Dim blocked tasks in agenda
(setq org-agenda-dim-blocked-tasks 'invisible)

;; =============================================================================
;; REPEATING TASKS
;; =============================================================================

;; Show next repeat occurrence
(setq org-agenda-show-future-repeats t)

;; Log repeating task completion
(setq org-log-repeat 'time)

;; =============================================================================
;; CUSTOM TODO FUNCTIONS
;; =============================================================================

;; Quick todo state changes
(defun funmacs/org-todo-next ()
  "Change TODO state to NEXT."
  (interactive)
  (org-todo "NEXT"))

(defun funmacs/org-todo-in-progress ()
  "Change TODO state to IN-PROGRESS."
  (interactive)
  (org-todo "IN-PROGRESS"))

(defun funmacs/org-todo-waiting ()
  "Change TODO state to WAITING."
  (interactive)
  (org-todo "WAITING"))

(defun funmacs/org-todo-done ()
  "Change TODO state to DONE."
  (interactive)
  (org-todo "DONE"))

(defun funmacs/org-todo-cancel ()
  "Change TODO state to CANCELLED."
  (interactive)
  (org-todo "CANCELLED"))

;; Automatically set NEXT when clocking in
(defun funmacs/org-clock-in-set-next ()
  "Set task to IN-PROGRESS when clocking in."
  (when (and (org-entry-is-todo-p)
             (not (string= (org-get-todo-state) "IN-PROGRESS")))
    (org-todo "IN-PROGRESS")))

(add-hook 'org-clock-in-hook #'funmacs/org-clock-in-set-next)

;; Automatically change sub-items parent state
(defun funmacs/org-update-parent-todo (n-done n-not-done)
  "Update parent TODO state based on children.
N-DONE is the number of completed items.
N-NOT-DONE is the number of not-done items."
  (let (org-log-done org-log-states)  ; turn off logging
    (org-todo (cond
               ((= n-not-done 0) "DONE")       ; all items complete
               ((= n-done 0) "TODO")           ; no items complete
               (t "IN-PROGRESS")))))           ; partially complete

(add-hook 'org-after-todo-statistics-hook #'funmacs/org-update-parent-todo)


;; =============================================================================
;; KEYBINDINGS
;; =============================================================================

(with-eval-after-load 'org
  ;; Fast TODO state changes
  (define-key org-mode-map (kbd "C-c t n") 'funmacs/org-todo-next)
  (define-key org-mode-map (kbd "C-c t i") 'funmacs/org-todo-in-progress)
  (define-key org-mode-map (kbd "C-c t w") 'funmacs/org-todo-waiting)
  (define-key org-mode-map (kbd "C-c t d") 'funmacs/org-todo-done)
  (define-key org-mode-map (kbd "C-c t c") 'funmacs/org-todo-cancel)
  
  ;; Priority setting
  (define-key org-mode-map (kbd "C-c p") 'org-priority)
  
  ;; Effort setting
  (define-key org-mode-map (kbd "C-c e") 'org-set-effort))

(provide 'funmacs-todo)

;;; funmacs-todo.el ends here
