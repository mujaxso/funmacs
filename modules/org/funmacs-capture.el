;;; funmacs-capture.el --- Complete Org Capture Configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive capture templates for quick note-taking and task collection

;;; Code:

;; =============================================================================
;; BASIC CAPTURE SETTINGS
;; =============================================================================

;; Default notes file
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; Use full frame for capture
(setq org-capture-use-agenda-date nil)

;; =============================================================================
;; CAPTURE TEMPLATES
;; =============================================================================

(setq org-capture-templates
      '(
        ;; -----------------------------------------------------------------------
        ;; TASKS AND TODO ITEMS
        ;; -----------------------------------------------------------------------
        
        ("t" "Tasks")
        
        ("tt" "Task" entry
         (file+headline "~/org/tasks.org" "Inbox")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
         :empty-lines 1)
        
        ("tn" "Next Action" entry
         (file+headline "~/org/tasks.org" "Next Actions")
         "* NEXT %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
         :empty-lines 1)
        
        ("ts" "Scheduled Task" entry
         (file+headline "~/org/tasks.org" "Scheduled")
         "* TODO %?\nSCHEDULED: %^t\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
         :empty-lines 1)
        
        ("td" "Deadline Task" entry
         (file+headline "~/org/tasks.org" "Deadlines")
         "* TODO %?\nDEADLINE: %^t\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
         :empty-lines 1)
        
        ;; -----------------------------------------------------------------------
        ;; NOTES
        ;; -----------------------------------------------------------------------
        
        ("n" "Notes")
        
        ("nn" "Quick Note" entry
         (file+headline "~/org/notes.org" "Quick Notes")
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%i\n%a"
         :empty-lines 1)
        
        ("nt" "Technical Note" entry
         (file+headline "~/org/notes.org" "Technical")
         "* %? :technical:\n:PROPERTIES:\n:CREATED: %U\n:SOURCE: %a\n:END:\n\n** Overview\n%i\n\n** Details\n\n** References\n"
         :empty-lines 1)
        
        ("nm" "Meeting Notes" entry
         (file+headline "~/org/meetings.org" "Meetings")
         "* %? :meeting:\n:PROPERTIES:\n:CREATED: %U\n:ATTENDEES:\n:END:\n\n** Agenda\n- \n\n** Notes\n%i\n\n** Action Items\n- [ ] \n\n** Next Steps\n"
         :empty-lines 1
         :clock-in t
         :clock-resume t)
        
        ;; -----------------------------------------------------------------------
        ;; JOURNAL
        ;; -----------------------------------------------------------------------
        
        ("j" "Journal")
        
        ("jj" "Journal Entry" entry
         (file+datetree "~/org/journal.org")
         "* %<%H:%M> %?\n%i\n"
         :empty-lines 1)
        
        ("jd" "Daily Review" entry
         (file+datetree "~/org/journal.org")
         "* Daily Review %<%Y-%m-%d>\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n** What went well\n- %?\n\n** What could be improved\n- \n\n** Tomorrow's focus\n- \n\n** Gratitude\n- "
         :empty-lines 1)
        
        ("jw" "Weekly Review" entry
         (file+datetree "~/org/journal.org")
         "* Weekly Review %<%Y-W%V>\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n** Accomplishments\n- %?\n\n** Challenges\n- \n\n** Lessons Learned\n- \n\n** Next Week Goals\n- "
         :empty-lines 1)
        
        ;; -----------------------------------------------------------------------
        ;; PROJECT MANAGEMENT
        ;; -----------------------------------------------------------------------
        
        ("p" "Projects")
        
        ("pp" "New Project" entry
         (file+headline "~/org/projects.org" "Projects")
         "* PROJECT %? [/]\n:PROPERTIES:\n:CREATED: %U\n:CATEGORY: %^{Category}\n:END:\n\n** Overview\n\n** Goals\n- [ ] \n\n** Milestones\n- [ ] \n\n** Resources\n\n** Notes\n"
         :empty-lines 1)
        
        ("pt" "Project Task" entry
         (file+headline "~/org/projects.org" "Tasks")
         "* TODO %?\n:PROPERTIES:\n:PROJECT: %^{Project}\n:CREATED: %U\n:END:\n%i\n%a"
         :empty-lines 1)
        
        ;; -----------------------------------------------------------------------
        ;; CODE AND DEVELOPMENT
        ;; -----------------------------------------------------------------------
        
        ("c" "Code")
        
        ("cc" "Code Snippet" entry
         (file+headline "~/org/code.org" "Snippets")
         "* %? :code:%^{Language}:\n:PROPERTIES:\n:CREATED: %U\n:LANGUAGE: %^{Language}\n:SOURCE: %a\n:END:\n\n#+BEGIN_SRC %^{Language}\n%i\n#+END_SRC\n\n** Description\n\n** Usage\n"
         :empty-lines 1)
        
        ("cb" "Bug Report" entry
         (file+headline "~/org/bugs.org" "Bugs")
         "* BUG %? :bug:\n:PROPERTIES:\n:CREATED: %U\n:PRIORITY: %^{Priority|A|B|C}\n:COMPONENT: %^{Component}\n:END:\n\n** Description\n%i\n\n** Steps to Reproduce\n1. \n\n** Expected Behavior\n\n** Actual Behavior\n\n** Environment\n- OS: NixOS\n- Version: \n\n** Additional Context\n%a"
         :empty-lines 1)
        
        ("ci" "Implementation Idea" entry
         (file+headline "~/org/ideas.org" "Development Ideas")
         "* IDEA %? :idea:\n:PROPERTIES:\n:CREATED: %U\n:EFFORT: %^{Effort|0:30|1:00|2:00|4:00|8:00}\n:END:\n\n** Problem\n\n** Proposed Solution\n%i\n\n** Benefits\n\n** Considerations\n"
         :empty-lines 1)
        
        ;; -----------------------------------------------------------------------
        ;; LINKS AND BOOKMARKS
        ;; -----------------------------------------------------------------------
        
        ("l" "Links")
        
        ("ll" "Link" entry
         (file+headline "~/org/links.org" "Unsorted")
         "* %? :link:\n:PROPERTIES:\n:CREATED: %U\n:URL: %^{URL}\n:END:\n%i"
         :empty-lines 1)
        
        ("lr" "Reading List" entry
         (file+headline "~/org/reading.org" "To Read")
         "* TODO %? :reading:\n:PROPERTIES:\n:CREATED: %U\n:URL: %^{URL}\n:AUTHOR: %^{Author}\n:END:\n\n** Summary\n%i\n\n** Why Read This\n"
         :empty-lines 1)
        
        ("lv" "Video to Watch" entry
         (file+headline "~/org/videos.org" "To Watch")
         "* TODO %? :video:\n:PROPERTIES:\n:CREATED: %U\n:URL: %^{URL}\n:DURATION: %^{Duration}\n:END:\n\n%i"
         :empty-lines 1)
        
        ;; -----------------------------------------------------------------------
        ;; PROTOCOL CAPTURES (Browser integration)
        ;; -----------------------------------------------------------------------
        
        ("w" "Web Captures")
        
        ("wp" "Protocol Link" entry
         (file+headline "~/org/links.org" "Web Captures")
         "* %:description :link:\n:PROPERTIES:\n:CREATED: %U\n:URL: %:link\n:END:\n\n%i"
         :empty-lines 1
         :immediate-finish t)
        
        ("wl" "Protocol Link with Selection" entry
         (file+headline "~/org/links.org" "Web Captures")
         "* %:description :link:\n:PROPERTIES:\n:CREATED: %U\n:URL: %:link\n:END:\n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?"
         :empty-lines 1)
        
        ;; -----------------------------------------------------------------------
        ;; QUICK CAPTURES
        ;; -----------------------------------------------------------------------
        
        ("q" "Quick Captures")
        
        ("qi" "Quick Inbox" item
         (file+headline "~/org/inbox.org" "Inbox")
         "- %?"
         :prepend t)
        
        ("qn" "Quick Note" plain
         (file+headline "~/org/notes.org" "Quick Notes")
         "%?\n%U"
         :empty-lines 1)
        
        ;; -----------------------------------------------------------------------
        ;; CONTACTS
        ;; -----------------------------------------------------------------------
        
        ("o" "Contact" entry
         (file+headline "~/org/contacts.org" "Contacts")
         "* %? :contact:\n:PROPERTIES:\n:NAME: %^{Name}\n:EMAIL: %^{Email}\n:PHONE: %^{Phone}\n:COMPANY: %^{Company}\n:CREATED: %U\n:END:\n\n** Notes\n"
         :empty-lines 1)
        
        ))

;; =============================================================================
;; CAPTURE HOOKS
;; =============================================================================

;; Automatically add CREATED property to new captures
(defun funmacs/org-capture-add-created-property ()
  "Add CREATED property with current timestamp if not present."
  (unless (org-entry-get nil "CREATED")
    (org-entry-put nil "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]"))))

(add-hook 'org-capture-before-finalize-hook #'funmacs/org-capture-add-created-property)

;; Save all org buffers after capture
(add-hook 'org-capture-after-finalize-hook
          (lambda ()
            (org-save-all-org-buffers)))

;; =============================================================================
;; CAPTURE KEYBINDINGS
;; =============================================================================

;; Global capture binding
(global-set-key (kbd "C-c c") 'org-capture)

;; Quick capture shortcuts (bypass menu)
(global-set-key (kbd "C-c C-x t") (lambda () (interactive) (org-capture nil "tt")))
(global-set-key (kbd "C-c C-x n") (lambda () (interactive) (org-capture nil "nn")))
(global-set-key (kbd "C-c C-x j") (lambda () (interactive) (org-capture nil "jj")))

;; =============================================================================
;; HELPER FUNCTIONS
;; =============================================================================

;; Create capture directories if they don't exist
(defun funmacs/ensure-org-capture-dirs ()
  "Create org capture directories if they don't exist."
  (let ((dirs '("~/org/")))
    (dolist (dir dirs)
      (unless (file-exists-p dir)
        (make-directory dir t)))))

(funmacs/ensure-org-capture-dirs)

;; Function to quickly open capture files
(defun funmacs/open-capture-file (file)
  "Open an org capture FILE."
  (interactive
   (list (completing-read "Open capture file: "
                          '("tasks.org" "notes.org" "journal.org"
                            "projects.org" "bugs.org" "links.org"
                            "reading.org" "inbox.org" "meetings.org")
                          nil t)))
  (find-file (concat org-directory "/" file)))

(global-set-key (kbd "C-c o f") 'funmacs/open-capture-file)

;; =============================================================================
;; CAPTURE TEMPLATES EXPANSION REFERENCE
;; =============================================================================

;; Template expansion sequences:
;; %?          - cursor position after template expansion
;; %t          - timestamp, date only
;; %T          - timestamp, date and time
;; %u          - inactive timestamp, date only
;; %U          - inactive timestamp, date and time
;; %i          - initial content (selected region)
;; %a          - annotation (link to current location)
;; %A          - like %a, but prompt for link description
;; %l          - link stored with org-store-link
;; %c          - current kill ring head
;; %x          - clipboard contents
;; %k          - title of currently clocked task
;; %K          - link to currently clocked task
;; %n          - user full name
;; %f          - file visited by current buffer
;; %F          - full path of file
;; %^g         - prompt for tags with completion
;; %^G         - prompt for tags without completion
;; %^t         - prompt for date
;; %^T         - prompt for date and time
;; %^u         - prompt for inactive date
;; %^U         - prompt for inactive date and time
;; %^C         - prompt for interactive selection from clipboard
;; %^L         - prompt for interactive selection of link
;; %^{PROMPT}  - prompt for text
;; %^{PROMPT|default|...} - prompt with completion

(provide 'funmacs-capture)

;;; funmacs-capture.el ends here
