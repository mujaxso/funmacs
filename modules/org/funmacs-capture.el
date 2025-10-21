;;; funmacs-capture.el --- Complete Org Capture Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive capture templates for quick note-taking and task collection.

;;; Code:

(require 'org)
(require 'org-capture)

;; -------------------------------
;; Basic capture settings
;; -------------------------------
(defvar funmacs/org-dir (or (bound-and-true-p org-directory) "~/org")
  "Fallback org directory if org-directory is unset.")
(setq org-directory funmacs/org-dir)
(setq org-default-notes-file (expand-file-name "notes.org" org-directory))
;; Use current date (not agenda date) by default; keep nil as desired
(setq org-capture-use-agenda-date nil)  ;; see agenda behavior docs

;; Ensure directories/files exist
(defun funmacs/ensure-org-capture-dirs ()
  "Ensure base org directory exists."
  (let ((dirs (list (file-name-as-directory org-directory))))
    (dolist (dir dirs)
      (unless (file-exists-p dir)
        (make-directory dir t)))))
(funmacs/ensure-org-capture-dirs)

;; -------------------------------
;; Hooks to enrich entries
;; -------------------------------
(defun funmacs/org-capture-add-created-property ()
  "Add CREATED property with current timestamp if not present."
  (unless (org-entry-get nil "CREATED")
    (org-entry-put nil "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]"))))
(add-hook 'org-capture-before-finalize-hook #'funmacs/org-capture-add-created-property)

(add-hook 'org-capture-after-finalize-hook #'org-save-all-org-buffers)

;; -------------------------------
;; Capture templates
;; -------------------------------
(setq org-capture-templates
      `(
        ;; Tasks
        ("t" "Tasks")

        ("tt" "Task" entry
         (file+headline ,(expand-file-name "tasks.org" org-directory) "Inbox")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
         :empty-lines 1)

        ("tn" "Next Action" entry
         (file+headline ,(expand-file-name "tasks.org" org-directory) "Next Actions")
         "* NEXT %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
         :empty-lines 1)

        ("ts" "Scheduled Task" entry
         (file+headline ,(expand-file-name "tasks.org" org-directory) "Scheduled")
         "* TODO %?\nSCHEDULED: %^t\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
         :empty-lines 1)

        ("td" "Deadline Task" entry
         (file+headline ,(expand-file-name "tasks.org" org-directory) "Deadlines")
         "* TODO %?\nDEADLINE: %^t\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
         :empty-lines 1)

        ;; Notes
        ("n" "Notes")

        ("nn" "Quick Note" entry
         (file+headline ,(expand-file-name "notes.org" org-directory) "Quick Notes")
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
         :empty-lines 1)

        ("nt" "Technical Note" entry
         (file+headline ,(expand-file-name "notes.org" org-directory) "Technical")
         "* %? :technical:\n:PROPERTIES:\n:CREATED: %U\n:SOURCE: %a\n:END:\n\n** Overview\n%i\n\n** Details\n\n** References\n"
         :empty-lines 1)

        ("nm" "Meeting Notes" entry
         (file+headline ,(expand-file-name "meetings.org" org-directory) "Meetings")
         "* %? :meeting:\n:PROPERTIES:\n:CREATED: %U\n:ATTENDEES:\n:END:\n\n** Agenda\n- \n\n** Notes\n%i\n\n** Action Items\n- [ ] \n\n** Next Steps\n"
         :empty-lines 1
         :clock-in t
         :clock-resume t)

        ;; Journal (modern file+olp+datetree; file+datetree still works but is deprecated in docs)
        ("j" "Journal")

        ("jj" "Journal Entry" entry
         (file+olp+datetree ,(expand-file-name "journal.org" org-directory))
         "* %<%H:%M> %?\n%i\n"
         :empty-lines 1)

        ("jd" "Daily Review" entry
         (file+olp+datetree ,(expand-file-name "journal.org" org-directory))
         "* Daily Review %<%Y-%m-%d>\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n** What went well\n- %?\n\n** What could be improved\n- \n\n** Tomorrow's focus\n- \n\n** Gratitude\n- "
         :empty-lines 1)

        ("jw" "Weekly Review" entry
         (file+olp+datetree ,(expand-file-name "journal.org" org-directory))
         "* Weekly Review %<%Y-W%V>\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n** Accomplishments\n- %?\n\n** Challenges\n- \n\n** Lessons Learned\n- \n\n** Next Week Goals\n- "
         :empty-lines 1)

        ;; Projects
        ("p" "Projects")

        ("pp" "New Project" entry
         (file+headline ,(expand-file-name "projects.org" org-directory) "Projects")
         "* PROJECT %? [/]\n:PROPERTIES:\n:CREATED: %U\n:CATEGORY: %^{Category}\n:END:\n\n** Overview\n\n** Goals\n- [ ] \n\n** Milestones\n- [ ] \n\n** Resources\n\n** Notes\n"
         :empty-lines 1)

        ("pt" "Project Task" entry
         (file+headline ,(expand-file-name "projects.org" org-directory) "Tasks")
         "* TODO %?\n:PROPERTIES:\n:PROJECT: %^{Project}\n:CREATED: %U\n:END:\n%i\n%a"
         :empty-lines 1)

        ;; Code and development
        ("c" "Code")

        ("cc" "Code Snippet" entry
         (file+headline ,(expand-file-name "code.org" org-directory) "Snippets")
         "* %? :code:%^{Language}:\n:PROPERTIES:\n:CREATED: %U\n:LANGUAGE: %^{Language}\n:SOURCE: %a\n:END:\n\n#+BEGIN_SRC %\\1\n%i\n#+END_SRC\n\n** Description\n\n** Usage\n"
         :empty-lines 1)

        ("cb" "Bug Report" entry
         (file+headline ,(expand-file-name "bugs.org" org-directory) "Bugs")
         "* BUG %? :bug:\n:PROPERTIES:\n:CREATED: %U\n:PRIORITY: %^{Priority|A|B|C}\n:COMPONENT: %^{Component}\n:END:\n\n** Description\n%i\n\n** Steps to Reproduce\n1. \n\n** Expected Behavior\n\n** Actual Behavior\n\n** Environment\n- OS: NixOS\n- Version: \n\n** Additional Context\n%a"
         :empty-lines 1)

        ("ci" "Implementation Idea" entry
         (file+headline ,(expand-file-name "ideas.org" org-directory) "Development Ideas")
         "* IDEA %? :idea:\n:PROPERTIES:\n:CREATED: %U\n:EFFORT: %^{Effort|0:30|1:00|2:00|4:00|8:00}\n:END:\n\n** Problem\n\n** Proposed Solution\n%i\n\n** Benefits\n\n** Considerations\n"
         :empty-lines 1)

        ;; Links and bookmarks
        ("l" "Links")

        ("ll" "Link" entry
         (file+headline ,(expand-file-name "links.org" org-directory) "Unsorted")
         "* %? :link:\n:PROPERTIES:\n:CREATED: %U\n:URL: %^{URL}\n:END:\n%i"
         :empty-lines 1)

        ("lr" "Reading List" entry
         (file+headline ,(expand-file-name "reading.org" org-directory) "To Read")
         "* TODO %? :reading:\n:PROPERTIES:\n:CREATED: %U\n:URL: %^{URL}\n:AUTHOR: %^{Author}\n:END:\n\n** Summary\n%i\n\n** Why Read This\n"
         :empty-lines 1)

        ("lv" "Video to Watch" entry
         (file+headline ,(expand-file-name "videos.org" org-directory) "To Watch")
         "* TODO %? :video:\n:PROPERTIES:\n:CREATED: %U\n:URL: %^{URL}\n:DURATION: %^{Duration}\n:END:\n\n%i"
         :empty-lines 1)

        ;; Protocol captures (browser integration)
        ("w" "Web Captures")

        ("wp" "Protocol Link" entry
         (file+headline ,(expand-file-name "links.org" org-directory) "Web Captures")
         "* %:description :link:\n:PROPERTIES:\n:CREATED: %U\n:URL: %:link\n:END:\n\n%i"
         :empty-lines 1
         :immediate-finish t)

        ("wl" "Protocol Link with Selection" entry
         (file+headline ,(expand-file-name "links.org" org-directory) "Web Captures")
         "* %:description :link:\n:PROPERTIES:\n:CREATED: %U\n:URL: %:link\n:END:\n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?"
         :empty-lines 1)

        ;; Quick captures
        ("q" "Quick Captures")

        ("qi" "Quick Inbox" item
         (file+headline ,(expand-file-name "inbox.org" org-directory) "Inbox")
         "- %?"
         :prepend t)

        ("qn" "Quick Note" plain
         (file+headline ,(expand-file-name "notes.org" org-directory) "Quick Notes")
         "%?\n%U"
         :empty-lines 1)

        ;; Contacts
        ("o" "Contact" entry
         (file+headline ,(expand-file-name "contacts.org" org-directory) "Contacts")
         "* %? :contact:\n:PROPERTIES:\n:NAME: %^{Name}\n:EMAIL: %^{Email}\n:PHONE: %^{Phone}\n:COMPANY: %^{Company}\n:CREATED: %U\n:END:\n\n** Notes\n"
         :empty-lines 1)
        ))

;; -------------------------------
;; Keybindings
;; -------------------------------
(global-set-key (kbd "C-c c") #'org-capture)

;; Convenience launchers
(global-set-key (kbd "C-c C-x t") (lambda () (interactive) (org-capture nil "tt")))
(global-set-key (kbd "C-c C-x n") (lambda () (interactive) (org-capture nil "nn")))
(global-set-key (kbd "C-c C-x j") (lambda () (interactive) (org-capture nil "jj")))

;; Quick file opener
(defun funmacs/open-capture-file (file)
  "Open an org capture FILE from org-directory."
  (interactive
   (list (completing-read "Open capture file: "
                          '("tasks.org" "notes.org" "journal.org"
                            "projects.org" "bugs.org" "links.org"
                            "reading.org" "inbox.org" "meetings.org" "videos.org" "ideas.org" "code.org")
                          nil t)))
  (find-file (expand-file-name file org-directory)))
(global-set-key (kbd "C-c o f") #'funmacs/open-capture-file)

(provide 'funmacs-capture)
;;; funmacs-capture.el ends here
