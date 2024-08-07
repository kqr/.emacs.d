;; XXX: Use-packageify this
;; Not sure why, but org got broken on a new machine (pulled in some parts that
;; were preinstalled and some parts from org-plus-contrib, but with different,
;; incompatible versions) and the solution may have been straight-pull-all
;; followed by straight-rebuild-all. May have been.

;; Load things also from contrib

;; Very confused by all of the following.
;; This used to be performed further down, which might have been an error.
(straight-use-package 'org)
(straight-use-package 'org-contrib)
(load "ox-rss.el")

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
;; Necessary to make org-emphasis-regexp-components take effect.
(org-reload)


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

(defun orgzly-link-entry-to-org-headline ()
  "Convert a link entry from Orgzly to a proper org headline."
  (interactive)
  (unless (outline-on-heading-p) (outline-previous-heading))
  (next-line)
  (let ((begin (point)))
    (outline-next-heading)
    (kill-region begin (point)))
  (outline-previous-heading)
  ;; XXX: Ugly hack to go to org entry title start.
  (forward-char 2)
  (kill-line)
  ;; Pop from kill-ring to avoid tainting it.
  (let ((link-title (pop kill-ring))
        (link-text (s-trim (pop kill-ring))))
    (org-insert-link nil link-text link-title)))

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

(use-package org-drill
  :after (org))

;; XXX: This doesn't work for some reason.
;;(require 'org-notmuch nil)

(when (require 'org-drill nil)
  (setq org-drill-left-cloze-delimiter "{{"
        org-drill-right-cloze-delimiter "}}"

        org-drill-leech-method 'skip
        ;; Avoid large batches of items followed by nothing for a few days
        ;; (This is apparently recommended with the default algorithm!)
        org-drill-add-random-noise-to-intervals-p t
        ;; Encourage frequent, shorter drills (tihs used to be 15 items per
        ;; session but at the current rate I usually have 30-ish flashcards per
        ;; day to go through so it would make sense to get them over with in
        ;; just one session. Still should not take more than 10 minutes unless
        ;; interrupted.)
        org-drill-maximum-duration 10
        org-drill-maximum-items-per-session 30
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
        ("~/org/flashcards.org" :maxlevel . 1)
        ("~/org/tickler.org" :maxlevel . 1)
        ("~/org/someday.org" :maxlevel . 3)
        ("~/org/notes.org" :maxlevel . 2)
        ("~/org/loop54.org" :maxlevel . 2)
        ("~/org/kth.org" :maxlevel . 2))
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-log-refile 'time
      org-reverse-note-order nil
      org-archive-location "~/org/archive.org::* %s")

;; Agenda
(setq org-agenda-files
      ;; For the time being, use org-agenda only for work stuff. Reintroduce
      ;; personal stuff
      '(;;"~/org/inbox.org"
        ;;"~/org/projects.org"
        ;;"~/org/tickler.org"
        "~/org/loop54.org"
        ;;"~/org/flashcards.org"
        ;;"~/org/kth.org"
        )
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
      ;; Older versions of org supported " " as the access key, but it
      ;; appears newer ones don't. Use "n" instead (home row, mnemonic: next.)
      '(("n" "Agenda"
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
                 (org-agenda-todo-ignore-scheduled t)))))))

;; Using Org to publish documents
(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (R . t)
                               (python . t)
                               (lisp . t)
                               (shell . t)
                               (dot . t)))

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
            "\\fi\n"))))

;; This is hopefully only required until I sort out the new straight.el setup.
;; These files (and potentially more) are not automatically loaded, which
;; causes some operations (like archiving) to fail for unknown definitions.
(let ((home (getenv "HOME")))
  (load (concat home "/.emacs.d/straight/repos/org/lisp/org.el"))
  (load (concat home "/.emacs.d/straight/repos/org/lisp/org-macs.el")))

;; Remove org-drill advice around org-get-tags that causes infinite recursion.
;; (Hopefully a temporary measure until the bug is fixed for realsies.)
(when (= 0 (remove-all-advice-for 'org-get-tags))
  (warn "No advice removed from org-get-tags. Maybe it's time to remove this workaround."))
