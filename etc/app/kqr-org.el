;; I /think/ these need to be set before Org is required
(setq-default org-export-backends '(org html publish s5 latex rss))
;; Allow longer sections of italics, and italicise mid-word with
;; zero width no break space
(setq-default org-emphasis-regexp-components
	      '("- ﻿\t('\"{"
		"- ﻿\t.,:!?;'\")}\\["
		" \t\r\n"
		"."
		8))


(autoload 'org-mode "org")
(autoload 'org-store-link "org")
(autoload 'org-agenda "org")
(autoload 'org-capture "org")

(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows
    intelligently.  Intelligently means: region, org-src-block,
    org-subtree, or defun, whichever applies first.  Narrowing to
    org-src-block actually calls `org-edit-src-code'.

    With prefix P, don't widen, just narrow even if buffer is already
    narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((and (boundp 'org-src-mode) org-src-mode (not p))
         (org-edit-src-exit))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'prog-mode)
         (save-excursion
           (cond ((or (outline-on-heading-p) (outline-previous-heading))
                  (outshine-narrow-to-subtree))
                 (t (narrow-to-defun)))))
        (t (error "Please select a region to narrow to"))))

(defun start-drill-session ()
  (interactive)
  (find-file "~/org/flashcards.org")
  (org-drill))

(defun capture-general-inbox (args)
  "Run inbox capture with ARGS."
  (interactive "P")
  (org-captudre args "i")
  (evil-insert-state))

(defun capture-mail-inbox (args)
  "Run mail capture with ARGS."
  (interactive "P")
  (org-capture args "m")
  (evil-insert-state))

(defun capture-flashcard (args)
  "Capture a new flashcard with ARGS for use with org-drill."
  (interactive "P")
  (org-capture args "f")
  (evil-insert-state))

(defun capture-general-inbox (args)
  "Run inbox capture with ARGS."
  (interactive "P")
  (org-capture args "i")
  (evil-insert-state))

(define-prefix-command 'kqr-org-prefix)
(define-key 'kqr-org-prefix (kbd "l") #'org-store-link)
(define-key 'kqr-org-prefix (kbd "RET") #'narrow-or-widen-dwim)
(define-key 'kqr-org-prefix (kbd "a") #'org-agenda)
(define-key 'kqr-org-prefix (kbd "i") #'capture-general-inbox)
(define-key 'kqr-org-prefix (kbd "m") #'capture-mail-inbox)
(define-key 'kqr-org-prefix (kbd "f") #'capture-flashcard)
(define-key 'kqr-org-prefix (kbd "d") #'start-drill-session)

;; This must happen last, when we have defined all keys in the prefix map
(define-key global-map (kbd "<f4>") 'kqr-org-prefix)

(with-eval-after-load "org"
  (require 'org-notmuch nil)

  (when (require 'org-drill nil)
    (setq org-drill-left-cloze-delimiter "{{"
          org-drill-right-cloze-delimiter "}}"

          org-drill-leech-method 'skip
          ;; Avoid large batches of items followed by nothing for a few days
          ;; (This is apparently recommended with the default algorithm!)
          org-drill-add-random-noise-to-intervals-p t
          ;; Encourage frequent, shorter drills
          org-drill-maximum-duration 10
          org-drill-maximum-items-per-session 15
          ;; Consider items recent until inter-repetition time is this big
          org-drill-days-before-old 15))

  (when (require 'calendar nil)
    (setq-default calendar-date-style 'iso))

  (defun org-mode-enable ()
    (setq fill-column 80))
  (add-hook 'org-mode-hook 'org-mode-enable)

  ;; Regular Org operation
  ;; Disable C-tab in org (some sort of forced archive toggle)
  ;; because it plays a better role with dired-sidebar
  (define-key org-mode-map (kbd "<C-tab>") nil)
  (setq org-return-follows-link t
        org-list-allow-alphabetical t
        org-hide-emphasis-markers nil
        org-fontify-quote-and-verse-blocks t
        org-ellipsis " ↴ "
        org-adapt-indentation t
        org-show-context-detail
        '((agenda . ancestors)
          (bookmark-jump . lineage)
          (isearch . lineage)
          (default . ancestors)))

  ;; TODO: Set faces for org-level-1 (1.618) and org-level-2 (1.618Q?)
  (when (require 'org-bullets)
    (setq org-bullets-bullet-list '("●" "■" "◆"))
    (add-hook 'org-mode-hook 'org-bullets-mode))

  (org-set-emph-re 'org-emphasis-regexp-components
                   org-emphasis-regexp-components)

  ;; Using Org as a planner
  (setq org-todo-keywords
        '((sequence "HOLD(h)" "WAIT(w)" "TODO(t)" "|")
          (sequence "|" "DONE(d)")
          (sequence "|" "CANCELED(c)"))

        org-todo-keyword-faces
        '(("HOLD" . (:foreground "dodger blue" :weight bold))
          ("WAIT" . (:foreground "black" :weight bold))
          ("TODO" . (:foreground "dark orange" :weight bold))
          ("DONE" . (:foreground "olivedrab3" :weight bold))
          ("CANCELED" . (:foreground "dim grey" :weight bold)))

        ;; Normally we'd want tasks to reset to HOLD, but since this is a
        ;; repeated task it also has a new scheduled date so it's okay if it
        ;; becomes a todo because it won't clutter until scheduled anyway!
        org-todo-repeat-to-state "TODO"

        org-enforce-todo-dependencies t
        org-hierarchical-todo-statistics nil

        ;; When closing an item, ask for a note – just in case there's an
        ;; important thought there that may otherwise not get recorded
        org-log-done 'note
        org-log-into-drawer t
        ;; Don't ask for a log message if cycling through with shift-arrow keys
        org-treat-S-cursor-todo-selection-as-state-change nil

        ;; Let's simplify this...
        ;; A = screamingly important, shall be done today
        ;; B = normal day-to-day "if you don't do this within a week or so, bad
        ;; things will happen."
        ;; C = fine if rescheduled indefinitely
        org-lowest-priority ?C
        org-default-priority ?C)

  (setq org-global-properties
        '(("Effort_ALL" . "0 0:15 0:30 1:00 2:00 4:00 6:00 8:00 12:00 16:00 24:00 36:00")))
  (setq org-columns-default-format
        (concat
         "%8TODO(State)"
         "%40ITEM(Task) "
         "%1PRIORITY(P) "
         "%9Effort(Estimated){:} "
         "%9CLOCKSUM(Worked) "
         "%TAGS(Tags)"))

  ;; Capturing, refiling and archiving
  (setq org-capture-templates
        '(("i" ">inbox" entry (file "") "* %?\n")
          ("m" "mail>inbox" entry (file "") "* %?\n%a\n")
          ("f" "mail>inbox" entry (file "~/org/flashcards.org") "* %? :drill:\n"))
        org-default-notes-file "~/org/inbox.org"
        org-refile-targets
        '(("~/org/projects.org" :maxlevel . 2)
          ("~/org/tickler.org" :maxlevel . 1)
          ("~/org/someday.org" :maxlevel . 3)
          ("~/org/notes.org" :maxlevel . 2)
          ("~/org/loop54.org" :maxlevel . 2)
          ("~/org/kth.org" :maxlevel . 2))
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil
        org-log-refile 'time
        org-reverse-note-order t
        org-archive-location "~/org/archive.org::* %s")

  ;; Agenda
  (setq org-agenda-files
        '("~/org/inbox.org"
          "~/org/projects.org"
          "~/org/tickler.org"
          "~/org/kth.org")
        org-agenda-dim-blocked-tasks 'invisible
        org-agenda-span 'day
        org-agenda-start-on-weekday nil
        org-agenda-use-time-grid nil
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t)

  (defun skip-living-projects ()
    "Skip top level trees that do have a TODO or WAIT child item"
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (case-fold-search nil))
      ;; FIXME: Check that the item is not scheduled!
      (and (re-search-forward "TODO\\|WAIT" subtree-end t)
           subtree-end)))

  (defun skip-entries-with-active-children ()
    "Skip top level trees that do have a TODO or WAIT child item"
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (case-fold-search nil))
      ;; FIXME: Check that the item is not scheduled!
      (and (re-search-forward "TODO\\|WAIT" subtree-end t)
           (org-end-of-line))))

  (setq org-agenda-custom-commands
        '((" " "Agenda"
           ((tags "FILE={projects.org}+LEVEL=1-noproject"
                  ((org-agenda-overriding-header "Stuck projects")
                   (org-agenda-skip-function #'skip-living-projects)))
            (tags "FILE={inbox.org}"
                  ((org-agenda-overriding-header "Inbox")))
            (agenda "" nil)
            (tags "-@out/TODO"
                  ((org-agenda-overriding-header "To do (not scheduled)")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Waiting")
                   (org-agenda-todo-ignore-scheduled t)))))
          ("w" "Work"
           ((agenda "" nil)
            (tags "/TODO"
                  ((org-agenda-overriding-header "To do (not scheduled)")
                   (org-agenda-skip-function
                    (lambda () (or (org-agenda-skip-entry-if 'scheduled)
                                   (skip-entries-with-active-children))))))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Waiting")
                   (org-agenda-todo-ignore-scheduled t))))
           ((org-agenda-files '("~/org/loop54.org"))))))

  ;; Using Org to publish documents
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (R . t)
                                 (python . t)
                                 (lisp . t)
                                 (shell . t)))

  (setq org-babel-python-command "python3"
        org-export-with-smart-quotes t
        org-export-with-emphasize t
        org-export-with-sub-superscripts nil
        org-export-with-footnotes t)

  (when (require 'ox-latex nil)
    (push (append '(("tufte-handout"
                     "\\documentclass[a4paper,11pt]{tufte-handout}"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}"))
                    ("tufte-book"
                     "\\documentclass[a4paper,10pt]{tufte-book}")))
          org-latex-classes)
    (setq org-latex-compiler "xelatex"
          org-latex-default-class "tufte-handout"
          org-latex-packages-alist
          ;; These depend on xelatex, so be careful with that!
          `(("" "fontspec" t)
            "\\setmainfont[Ligatures=TeX,Numbers=OldStyle]{Whitman}"
            ("AUTO" "polyglossia" t)
            ("" "pdflscape" t)
            ("" "pseudo-ewd" t)
            ;; tufte-handout xelatex shim
            ,(concat
              "\\ifxetex\n"
              "  \\newcommand{\\textls}[2][5]{%\n"
              "    \\begingroup\\addfontfeatures{LetterSpace=#1}#2\\endgroup\n"
              "  }\n"
              "  \\renewcommand{\\allcapsspacing}[1]{\\textls[15]{#1}}\n"
              "  \\renewcommand{\\smallcapsspacing}[1]{\\textls[0]{#1}}\n"
              "  \\renewcommand{\\allcaps}[1]{\\textls[15]{\\MakeTextUppercase{#1}}}\n"
              " \\renewcommand{\\smallcaps}[1]{\\smallcapsspacing{\\scshape\\MakeTextLowercase{#1}}}\n"
              " \\renewcommand{\\textsc}[1]{\\smallcapsspacing{\\textsmallcaps{#1}}}\n"
              "\\fi\n")))))

;; for some reason, this started misbehaving after Caspian jumped up on my
;; keyboard, so I have made it temporarly non-mandatory and at some point
;; I'll have to troubleshoot what went wrong.
(when (and nil (require 'org-trello))
  ;; Some hacks to make org-trello work with boards with large boards.
  ;; The implementation is not very pretty, and it's hacky of me to re-define
  ;; the functions rather than advise them, but meh.

  (defun orgtrello-api-get-full-cards-from-page (board-id &optional before-id)
    "Create a paginated retrieval of 25 cards before BEFORE-ID from BOARD-ID."
    (orgtrello-api-make-query
     "GET"
     (format "/boards/%s/cards" board-id)
     `(("actions" .  "commentCard")
       ("checklists" . "all")
       ("limit" . "250")
       ("before" . ,(or before-id ""))
       ("filter" . "open")
       ("fields" .
        "closed,desc,due,idBoard,idList,idMembers,labels,name,pos"))))

  (defun orgtrello-controller--retrieve-full-cards (data &optional before-id)
    "Retrieve the full cards from DATA, optionally paginated from before-ID.
DATA is a list of (archive-cards board-id &rest buffer-name point-start).
Return the cons of the full cards and the initial list."
    (-let* (((archive-cards board-id &rest) data)
            (cards
             (-> board-id
                (orgtrello-api-get-full-cards-from-page before-id)
                (orgtrello-query-http-trello 'sync)))
            (more-cards
             (when cards
               (let ((before-id (car (sort (mapcar 'orgtrello-data-entity-id cards) 'string<))))
                 (car (orgtrello-controller--retrieve-full-cards-paged data before-id))))))
      (cons (append more-cards cards) data))))
