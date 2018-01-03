;;; init-common.el --- Configuration common to all my Emacs installations
;;
;;; Commentary:
;; Testing I18N: Räksmörgås
;;
;;; Code:
;;; Built-in emacs config
(use-package emacs
  :init
  (setcq inhibit-startup-screen t)
  (setcq initial-scratch-message "")
  (setcq major-mode 'text-mode)
  (setcq make-backup-files nil)
  (setcq large-file-warning-threshold 100000000)

  ;; Prevent Emacs from mixing tabs and spaces...
  (setcq indent-tabs-mode nil)

  ;; Remove a bunch of distracting, unnecessary, silly graphic components
  (when (display-graphic-p)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (tooltip-mode -1))
  (menu-bar-mode -1)
  (blink-cursor-mode -1)

  (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
  (add-to-list 'default-frame-alist '(tool-bar-lines . nil))
  (add-to-list 'initial-frame-alist '(left-fringe . 40))
  (add-to-list 'default-frame-alist '(left-fringe . 40))
  (add-to-list 'initial-frame-alist '(right-fringe . 80))
  (add-to-list 'default-frame-alist '(right-fringe . 80))

  (setcq cursor-in-non-selected-windows nil)
  (global-linum-mode -1)
  ;; Enable column numbers in mode line (probably unnecessary with my
  ;; theme since it overrides the mode line anyway...)
  (column-number-mode +1)

  ;; We like our theme
  (setcq frame-background-mode 'light)
  (load-theme 'modern-minik)

  ;;;; Set variable width font for most things (but not quite all of them!)
  (when (display-graphic-p)
    (set-frame-font (font-spec :name "Linux Libertine O" :size 11.0) t t)
    (custom-theme-set-faces 'user '(fixed-pitch
                                    ((t :family "Luxi Mono" :height 0.8))))
    (add-to-list 'initial-frame-alist '(line-spacing . 1))
    (add-to-list 'default-frame-alist '(line-spacing . 1)))
  
  ;; Don't soft wrap lines, simply let them extend beyond the window width
  (setcq truncate-lines t)
  (set-display-table-slot standard-display-table 0 ?›)
  
  ;; Set a column limit at 80 characters
  (setcq fill-column 80)
  ;; Automatically wrap content extending beyond this
  (setq-default auto-fill-function 'do-auto-fill)
  
  ;; Copy stuff to the X11 primary selection
  (setcq select-enable-primary t)

  ;; Make available smaller changes in text size
  (setcq text-scale-mode-step 1.05)

  ;; Focus on newly opened help windows
  (setcq help-window-select t)

  (setcq browse-url-browser-function #'eww)

  (setcq user-full-name "Christoffer Stjernlöf")
  (setcq user-mail-address "k@rdw.se")

  ;;;;; Basic keybind setup

  ;; C-z defaults to suspend-frame which behaves weirdly and is never necessary
  (unbind-key "C-z")

  ;; <f1> defaults to duplicate the functionality provided by C-h
  ;; By unbinding C-h we open it up to be used for better things
  (unbind-key "C-h")

  ;; Allow us to keep ctrl held down for common chords
  (bind-key* "C-x C-0" #'delete-window)
  (bind-key* "C-x C-1" #'delete-other-windows)
  (bind-key* "C-x C-b" #'switch-to-buffer)
  (bind-key* "C-x C-o" #'other-window)

  ;; This is neat to quickly go back to the previous buffer
  (bind-key* "C-q" #'kill-this-buffer)
  ;; But then we also need this...
  (bind-key* "C-S-q" #'quoted-insert)

  ;; ...this is pretty neat!
  (use-package calc
    :bind (("C-=" . quick-calc))
    :config
    (setcq calc-multiplication-has-precedence nil))

  ;; this could be even more neat...
  (use-package calc
    :bind (("<f12>" . calc)))
  
  ;; Make "join this line to the one above" a bit more convenient to perform
  (bind-key* "C-J" #'delete-indentation)

  ;; This is so neat too! Automatically highlight conflicts in files :3
  (use-package smerge-mode :config
    (defun sm-try-smerge ()
      "Start smerge-mode when a git conflict is detected."
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil t)
          (smerge-mode 1))))

    (add-hook 'find-file-hook 'sm-try-smerge t)))


;;; External packages
;;;; Enable easy troubleshooting of init file
(use-package bug-hunter :commands bug-hunter-init-file)
;;;; UI configuration should appear early

;; Use variable-width fonts yeaah
(use-package unicode-fonts :init
  (use-package persistent-soft :config
    (unicode-fonts-setup))
  :config
  (setcq unicode-fonts-overrides-mapping
         (append
          '((#x0000 #x21ff ("Linux Libertine O" "Symbola"))
            (#x2200 #xffff ("Symbola")))
          unicode-fonts-overrides-mapping)))

;; Attempt to set fixed with fonts for buffers that need them...
(use-package face-remap :config
  (setcq buffer-face-mode-face '(:inherit fixed-pitch))

  (add-hook 'calendar-mode-hook #'buffer-face-mode)
  (add-hook 'notmuch-tree-mode-hook #'buffer-face-mode))

;; Highlight text extending beyond 80 characters
(use-package column-enforce-mode :diminish column-enforce-mode :config
  ;; inherit fill-column
  (setcq column-enforce-column nil)
  (setcq column-enforce-should-enable-p
         (lambda () (string-match "^notmuch" (symbol-name major-mode))))
  (global-column-enforce-mode +1))

;; Highlight FIXME TODO etc. in comments
(use-package fic-mode :init
  ;; This needs to be made globalized
  (fic-mode +1)
  ;; Temporary solution...
  (add-hook 'find-file-hook #'fic-mode)
  :config
  (setcq fic-highlighted-words (split-string "FIXME TODO BUG XXX")))

;; Highlight the stuff between matching parentheses ;; Currently disabled
;; because versor-mode does this so much better
(use-package paren
  :disabled
  :config
  (show-paren-mode +1)
  (setcq show-paren-delay 0)
  (setcq show-paren-when-point-inside-paren t)
  (setcq show-paren-style 'expression)
  ;; This is in order for region to take priority over show-paren highlighting
  (setcq show-paren-priority -200))

;; Try to keep the buffer scrolled so the cursor is centered
(use-package centered-cursor-mode :diminish centered-cursor-mode :config
  (global-centered-cursor-mode +1))

(use-package popup)

;; Avoid the built-in Emacs window manager and try to use the real one
(use-package frames-only-mode :config
  (frames-only-mode))
;;;; Navigation and fuzzy finding
(use-package ivy :diminish ivy-mode :config
  (setcq ivy-initial-inputs-alist nil)
  (ivy-mode +1))
(use-package counsel :bind
  (("M-x" . counsel-M-x)))

(use-package smex)
;;;;; Org-like structuring of any document
(use-package outshine
  :bind (("M-n" . outshine-narrow-to-subtree)
         ("M-h" . widen))
  :commands (outline-minor-mode)
  :init
  (add-hook 'outline-minor-mode-hook #'outshine-hook-function)
  (add-hook 'prog-mode-hook #'outline-minor-mode)
  :config
  (setcq outshine-startup-folded-p t)
  ;; Allow narrowing to subtree even when inside subtree
  (advice-add 'outshine-narrow-to-subtree :before
              (lambda (&rest args) (unless (outline-on-heading-p t)
                                     (outline-previous-visible-heading 1)))))

;;;;; Versor-mode for convenient navigation
(use-package versor
  :ensure nil
  :config
  (require 'versor)
  (setcq versor-auto-change-for-modes nil)
  (setcq versor-move-out-when-at-end nil)
  (setcq versor-level-wrap nil)
  (setcq versor-meta-level-wrap nil)
  (setcq versor-text-level 2)
  (setcq versor-text-meta-level 4)
  (setcq versor-non-text-level 2)
  (setcq versor-non-text-meta-level 6)

  (setcq versor-meta-dimensions-valid-for-modes
         '((t t "cartesian" "structural" "text" "structured text" "program")))

  (setcq versor-mode-current-levels
         (mapcar #'versor-mode-levels-triplet
                 '((emacs-lisp-mode "structural" "exprs")
                   (lisp-interaction-mode "structural" "exprs")
                   (c-mode "program" "statement-parts")
                   (text-mode "text" "words")
                   (message-mode "text" "words")
                   (org-mode "text" "words"))))

  (versor-setup
   'arrows
   'arrows-misc
   'meta
   'ctrl-x
   'text-in-code
   'quiet-underlying
   'local)

  (let ((color (face-attribute 'show-paren-match :background)))
    (set-face-attribute 'versor-item-face nil :inherit 'unspecified :background color)
    (seq-doseq (metamovemap (seq-subseq moves-moves 1))
      (seq-doseq (movemap (seq-subseq metamovemap 1))
        (versor-define-move movemap 'color color)
        (versor-define-move movemap :background color)))))

;;;; God mode
;; Modal editing *is* the greatest. This reduces hand-strain in an Emacs
;; default friendly sort of way. Very cool, actually.
(use-package god-mode
  :defines god-mode-isearch-map
  :bind (("<escape>" . god-local-mode)
         :map isearch-mode-map ("<escape>" . god-mode-isearch-activate)
         :map god-mode-isearch-map ("<escape>" . god-mode-isearch-disable))

  :demand t

  :init
  (setcq god-exempt-predicates '(god-exempt-mode-p god-special-mode-p))
  (setcq god-exempt-major-modes
         '(org-agenda-mode
           dired-mode
           sr-mode
           eshell-mode
           doc-view-mode
           magit-popup-mode
           calc-mode
           Custom-mode
           notmuch-hello-mode
           notmuch-search-mode
           notmuch-show-mode
           notmuch-tree-mode))
  
  (defun god-exempt-mode-p ()
    "Return non-nil if major-mode is exempt or inherits from exempt mode."
    (or (memq major-mode god-exempt-major-modes)
        (seq-some (lambda (exempt) (god-mode-child-of-p major-mode exempt))
                  god-exempt-major-modes)))
  
  :config
  (god-mode-all)
  (require 'god-mode-isearch)

  (defun god-mode-update-cursor (&optional _)
    "Make sure the modeline and cursor is updated with god mode state."
    (if (or god-local-mode buffer-read-only) (setq cursor-type 'box)
      (setq cursor-type 'bar))
    (if god-local-mode
        (set-face-background 'mode-line
                             (face-attribute 'font-lock-type-face :foreground))
      (mapc #'enable-theme custom-enabled-themes)))

  (add-hook 'god-mode-enabled-hook #'god-mode-update-cursor)
  (add-hook 'god-mode-disabled-hook #'god-mode-update-cursor)
  (add-hook 'read-only-mode-hook #'god-mode-update-cursor)

  ;; I have barely any idea what I'm doing here... I'm just spamming these
  ;; to ensure the modeline is updated timely...
  (add-hook 'after-change-major-mode-hook #'god-mode-update-cursor)
  (add-hook 'window-configuration-change-hook #'god-mode-update-cursor)
  (add-hook 'mode-selection-hook #'god-mode-update-cursor)
  (add-hook 'buffer-selection-hook #'god-mode-update-cursor)
  (add-hook 'find-file-hook #'god-mode-update-cursor)

  (defun god-has-priority ()
    "Try to ensure that god mode keeps priority over other minor modes."
    (unless (and (consp (car minor-mode-map-alist))
                 (eq (caar minor-mode-map-alist) 'god-local-mode-map))
      (let ((godkeys (assq 'god-local-mode minor-mode-map-alist)))
        (assq-delete-all 'god-local-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist godkeys))))
  (add-hook 'god-mode-enabled-hook #'god-has-priority))

(use-package tab-as-escape :diminish tab-as-escape-mode
  :ensure nil
  :config
  (require 'tab-as-escape)
  (tab-as-escape-mode +1))

;;;; Editing, general
(use-package autorevert :config
  ;; Don't automatically reload files from disk without asking
  ;; TODO: This needs to be included in a bunch of hooks
  (global-auto-revert-mode -1))

(use-package undo-tree :diminish undo-tree-mode :config
  (global-undo-tree-mode)
  (setcq undo-tree-visualizer-diff t))

(use-package visual-regexp :config
  (use-package visual-regexp-steroids :bind
    (("C-%" . #'vr/query-replace)
     ("C-s" . #'vr/isearch-forward)
     ("C-r" . #'vr/isearch-backward))))

(use-package dabbrev :config
  (bind-key* "C-M-i" 'dabbrev-expand)
  (setcq dabbrev-case-fold-search nil))

(use-package yasnippet :init
  (setcq yas-snippet-dirs '("~/.emacs.d/etc/snippets"))
  :config
  (yas-global-mode 1))

;;;; Programming, general
;;;;; Edit by balanced parentheses
(use-package paredit
  :disabled
  :diminish paredit-mode :config
  ;; Fixes to make paredit more convenient to work with in other languages
  (setcq paredit-space-for-delimiter-predicates
         (list (lambda (&rest args) nil)))
  (unbind-key "\\" paredit-mode-map)
  (unbind-key "M-q" paredit-mode-map)
  
  (add-hook 'text-mode-hook #'paredit-mode)
  (add-hook 'prog-mode-hook #'paredit-mode))

;;;;; Indentation/whitespace stuff
(use-package aggressive-indent :diminish aggressive-indent-mode :config
  (electric-indent-mode -1)
  (global-aggressive-indent-mode +1))

(use-package whitespace :bind
  ("C-z" . whitespace-mode)

  :init
  (defvar ws-show-paren-mode-active nil)
  
  :config
  (defun toggle-showparen-with-whitespace (arg)
    "Ensures show-paren-mode is off when whitespace-mode is turned on."
    ;; Initialise variable
    (unless (boundp 'ws-show-paren-mode-active)
      (setq ws-show-paren-mode-active (bound-and-true-p show-paren-mode)))

    ;; Do check
    (if (not (bound-and-true-p whitespace-mode))
        ;; Whitespace mode turned off
        (when ws-show-paren-mode-active (show-paren-mode +1))
      
      ;; Whitespace mode turned on, check if paren-mode is active
      (setq ws-show-paren-mode-active (bound-and-true-p show-paren-mode))
      (when ws-show-paren-mode-active (show-paren-mode -1))))

  (advice-add 'whitespace-mode :after #'toggle-showparen-with-whitespace)

  (setcq whitespace-style
         '(face trailing tabs spaces newline space-mark tab-mark newline-mark))
  (setcq whitespace-display-mappings
         '((space-mark 32 [183] [46])
           (tab-mark 9 [187 9] [92 9])
           (newline-mark 10 [182 10]))))

(use-package flycheck :diminish flycheck-mode :config
  (global-flycheck-mode 1))

;;;;; Project management of source code etc.
(use-package projectile
  :config
  (projectile-mode)
  (use-package counsel-projectile :config
    (counsel-projectile-mode)))

;;;; Programming, language-specific
(use-package cc-mode :config
  (setcq c-default-style "stroustrup")
  (setcq c-basic-offset 4))

(use-package nxml-mode :mode "\\.html"
  :ensure nil :config
  (use-package html5-schema))

(use-package intero :config
  (add-hook 'haskell-mode-hook #'intero-mode))

(use-package ada-mode)

(use-package ess :init (require 'ess-site))

;;;; Prose
(use-package synosaurus :bind
  (("C-@" . synosaurus-choose-and-replace))
  :config
  (setcq synosaurus-choose-method 'popup)
  (add-hook 'text-mode-hook #'synosaurus-mode))

;;;; Miscellaneous
;;;;; Export window contents to neat HTML
(use-package htmlize
  :commands (htmlize-buffer htmlize-file htmlize-many-files htmlize-region))

;;;;; Analyse command usage frequency to optimise config
(use-package keyfreq :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))
;;;; EAAS = Emacs-As-An-(operating)-System
;;;;; File manager
(unbind-key "<f2>")
(use-package sunrise-commander :bind
  (("<f2>" . sunrise-cd))

  :config
  (add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode)))

;;;;; Git integration
(use-package magit :bind
  (("<f3>" . magit-status)))

;;;;; Organizer, planner, note taking etc.
(use-package calendar :config
  (setcq calendar-date-style 'iso))

(unbind-key "<f4>")
(use-package org
  :ensure org-plus-contrib
  :bind (("<f4> a" . org-agenda)
         ("<f4> c" . org-capture)
         ("<f4> o" . org-cycle-agenda-files)
         ("<f4> l" . org-store-link))
  :demand t
  :mode ("\\.org\\'" . org-mode)

  :defines org-capture-templates org-latex-classes

  :init
  (setcq org-export-backends '(org html publish s5 latex rss))
  
  :config
  ;;;;;; Regular Org operation
  (setcq org-return-follows-link t)
  (setcq org-list-allow-alphabetical t)
  
  ;; Allow longer sections of italics, and italicise mid-word with
  ;; zero width no break space
  (let ((pre (nth 0 org-emphasis-regexp-components)))
    ;; If non-breaking space is in pre, don't bother adding it
    (setcar (nthcdr 0 org-emphasis-regexp-components)
            (if (string-match "﻿" pre) pre (concat pre "﻿")))
    (setcar (nthcdr 4 org-emphasis-regexp-components)
            8)
    (org-set-emph-re 'org-emphasis-regexp-components
                     org-emphasis-regexp-components))

  ;;;;;; Using Org as a planner
  ;; Let me copy emails as links in Org
  (require 'org-notmuch)
  
  ;; separate sets to avoid accidentally completing something (for example)
  (setcq org-todo-keywords
         '((sequence "HOLD(h)" "WAIT(w)" "TODO(t)" "|")
           (sequence "|" "DONE(d)")
           (sequence "|" "CANCELED(c)")))

  (setcq org-todo-keyword-faces
         '(("HOLD" . (:foreground "dodger blue" :weight bold))
           ("WAIT" . (:foreground "black" :weight bold))
           ("TODO" . (:foreground "dark orange" :weight bold))
           ("DONE" . (:foreground "olivedrab3" :weight bold))
           ("CANCELED" . (:foreground "dim grey" :weight bold))))

  ;; Normally we'd want tasks to reset to HOLD, but since this is a
  ;; repeated task it also has a new scheduled date so it's okay if it
  ;; becomes a todo because it won't clutter until scheduled anyway!
  (setcq org-todo-repeat-to-state "TODO")

  (setcq org-hierarchical-todo-statistics nil)

  ;; When closing an item, ask for a note – just in case there's an
  ;; important thought there that may otherwise not get recorded
  (setcq org-log-done 'note)
  ;; Don't ask for a log message if cycling through with shift-arrow keys
  (setcq org-treat-S-cursor-todo-selection-as-state-change nil)

  ;; Let's simplify this...
  ;; A = screamingly important
  ;; B = normal day-to-day "you should do this or bad things will happen"
  ;; C = fine if rescheduled
  (setcq org-lowest-priority ?C)
  (setcq org-default-priority ?B)

  ;; Capturing and refiling
  (setcq org-capture-templates
         '(("i" ">inbox" entry (file "") "* %?\n")))
  (setcq org-default-notes-file "~/org/inbox.org")
  (setcq org-refile-targets
         '(("~/org/projects.org" :maxlevel . 3)
           ("~/org/tickler.org" :maxlevel . 1)
           ("~/org/someday.org" :maxlevel . 3)
           ("~/org/notes.org" :maxlevel . 2)))
  (setcq org-refile-allow-creating-parent-nodes 'confirm)
  (setcq org-refile-use-outline-path t)
  (setcq org-outline-path-complete-in-steps nil)
  (setcq org-log-refile 'time)
  (setcq org-reverse-note-order t)

  ;; Agenda and archiving
  (setcq org-agenda-files '("~/org/inbox.org" "~/org/projects.org" "~/org/tickler.org"))
  (setcq org-archive-location "~/org/archive.org::* %s")

  (setcq org-enforce-todo-dependencies t)
  (setcq org-agenda-dim-blocked-tasks 'invisible)
  (setcq org-agenda-span 'day)
  (setcq org-agenda-skip-scheduled-if-done t)

  (defun skip-living-projects ()
    "Skip top level trees that do have a TODO or WAIT child item"
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (case-fold-search nil))
      (if (re-search-forward "TODO\\|WAIT" subtree-end t)
          subtree-end
        nil)))

  (setcq org-agenda-custom-commands
         '((" " "Agenda"
            ((tags "FILE={inbox.org}"
                   ((org-agenda-overriding-header "Inbox")))
             (agenda "" nil)
             (tags "-@out/TODO"
                   ((org-agenda-overriding-header "To do (not scheduled)")
                    (org-agenda-todo-ignore-scheduled t)))
             (todo "WAIT"
                   ((org-agenda-overriding-header "Waiting")
                    (org-agenda-todo-ignore-scheduled t)))
             (tags "FILE={projects.org}+LEVEL=1-noproject"
                   ((org-agenda-overriding-header "Stuck projects")
                    (org-agenda-skip-function #'skip-living-projects)))))))

  ;; TODO: set more/better custom agenda commands for various contexts


  ;;;;;; Using Org to publish documents
  ;; allow execution of R code in org (for neat graphs and tables and stuff!)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t) (R . t)))

  (setcq org-export-with-smart-quotes t)
  (setcq org-export-with-emphasize t)
  (setcq org-export-with-sub-superscripts nil)
  (setcq org-export-with-footnotes t)

  (require 'ox-latex)
  (setcq org-latex-classes
         (append '(("tufte-handout"
                    "\\documentclass[a4paper,11pt]{tufte-handout}"
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" . "\\subsection*{%s}"))
                   ("tufte-book"
                    "\\documentclass[a4paper,10pt]{tufte-book}"))
                 org-latex-classes))

  (setcq org-latex-compiler "xelatex")
  (setcq org-latex-default-class "tufte-handout")
  (setcq org-latex-packages-alist
         ;; These depend on xelatex, so be careful with that!
         `(("" "fontspec" t)
           "\\setmainfont[Numbers=OldStyle]{Whitman}"
           ("AUTO" "polyglossia" t)
           ("" "pdflscape" t)
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


;;;;; Email client
(use-package notmuch
  :bind (("<f5>" . notmuch))
  ;; Load the locally installed notmuch mode to ensure versions match
  :ensure nil :init
  (defun notmuch-toggle-deleted-tag (&optional beg end)
    (interactive (notmuch-search-interactive-region))
    (if (member "deleted" (notmuch-search-get-tags))
        (notmuch-search-tag (list "-deleted") beg end)
      (notmuch-search-tag (list "+deleted") beg end)))
  
  :config
  (setcq notmuch-search-line-faces '(("unread" :weight bold)))
  (setcq notmuch-show-indent-messages-width 4)
  (setcq notmuch-search-oldest-first nil)

  (setcq notmuch-archive-tags '("-inbox" "-unread"))
  (define-key notmuch-search-mode-map "k" #'notmuch-toggle-deleted-tag)

  (setcq notmuch-poll-script nil)

  (setcq notmuch-hello-sections
         '(notmuch-hello-insert-saved-searches
           notmuch-hello-insert-search
           notmuch-hello-insert-recent-searches
           notmuch-hello-insert-alltags
           notmuch-hello-insert-footer))

  (setcq notmuch-saved-searches
         '((:name "inbox" :query "tag:inbox" :key "i")
           (:name "unread" :query "tag:unread" :key "u")
           (:name "spam" :query "tag:spam" :key "m")
           (:name "sent" :query "tag:sent" :key "s")
           (:name "all mail" :query "*" :key "a")))

  ;;;;;; Message mode for composing emails
  (setcq message-cite-function 'message-cite-original-without-signature)
  (setcq message-citation-line-function 'message-insert-formatted-citation-line)
  (setcq message-cite-reply-position 'traditional)
  (setcq message-yank-prefix "> ")
  (setcq message-yank-cited-prefix ">")
  (setcq message-yank-empty-prefix ">")
  (setcq message-citation-line-format "%N (%n) %Y-%m-%d:")
  (setcq message-auto-save-directory "~/mail/drafts")
  (setcq message-default-mail-headers "Cc: \nBcc: \n")
  (setcq message-kill-buffer-on-exit t)
  (setcq message-sendmail-envelope-from 'header)
  (setcq message-send-mail-function 'message-send-mail-with-sendmail)
  (setcq message-confirm-send t)
  (setcq message-hidden-headers
         '("^User-Agent:" "^Face:" "^X-Face:" "^X-Draft-From"))
  (setcq mail-specify-envelope-from t)
  (setcq send-mail-function 'smtpmail-send-it)

  ;; Always sign outgoing messages
  (add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)

  ;;;;;; Sendmail integration
  (use-package sendmail :config
    (setcq mail-envelope-from 'header)
    (setcq sendmail-program "/usr/bin/msmtp")))



;;;; Diminish a bunch of modes according to the theme
;; We do tihs here because apparently use-package will override it otherwise
(dolist (mode (mapcar #'car modern-minik-mode-icon-alist))
  (unless (member mode '(flycheck-mode))
    (diminish mode (modern-minik-mode-icon mode))))


;;; init-common.el ends here
