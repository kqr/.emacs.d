;;; init-common.el --- Configuration common to all my Emacs installations
;;
;;; Commentary:
;; Testing I18N: Räksmörgås
;;
;;; Code:
(eval-and-compile (push "~/.emacs.d/etc" load-path))
(require 'kqr-load-path)
(require 'setcq)

;;; Built-in emacs config
(use-package emacs
  :custom
  (inhibit-startup-screen t)
  (initial-scratch-message "")
  (major-mode 'org-mode "Org is useful default for scratch buffers")
  (make-backup-files nil)
  (large-file-warning-threshold 100000000)

  (auto-window-vscroll nil "Reduces lag, I think")
  (scroll-conservatively 101 "Reduces lag, I think")
  (line-move-visual nil "Reduces lag, I think")

  (indent-tabs-mode nil "Prevent Emacs from mixing tabs and spaces.")
  (sentence-end-double-space nil "No need to fake typesetting.")

  (cursor-in-non-selected-windows nil "Hide cursor in inactive windows")

  (fill-column 80 "Set a column limit at 80 characters")
  (truncate-lines t "Let text extend beyond the window width")
  (auto-fill-function 'do-auto-fill "Automatically hard wrap content instead")
  
  (select-enable-primary t "Copy stuff to the X11 primary selection")
  (delete-selection-mode 1 "Typing stuff with active region replaces region")

  (text-scale-mode-step 1.05 "Make available smaller changes in text size")

  (help-window-select t "Focus on newly opened help windows")

  (browse-url-browser-function #'eww)

  (user-full-name "Christoffer Stjernlöf")
  (user-mail-address "k@rdw.se")

  
  :init
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

  ;; Replace the default line-extends-beyond-window symbol
  (set-display-table-slot standard-display-table 0 ?›)
  

  ;;;;; Basic keybind setup
  ;; C-z defaults to suspend-frame which behaves weirdly and is never necessary
  (unbind-key "C-z")

  ;; <f1> defaults to duplicate the functionality provided by C-h
  ;; By unbinding C-h we open it up to be used for better things
  (unbind-key "C-h")

  ;; This is neat to quickly go back to the previous buffer
  (bind-key* "C-q" #'kill-this-buffer)
  ;; But then we also need this...
  (bind-key* "C-S-q" #'quoted-insert)

  ;; Make "join this line to the one above" a bit more convenient to perform
  (bind-key* "C-S-j" #'delete-indentation))

;;; Config troubleshooting
(use-package bug-hunter :commands bug-hunter-init-file)

;;; UI configuration (should appear early)
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

;; Let's see how this feels! Replace keywords with Unicode counterpart
(global-prettify-symbols-mode +1)
(setq prettify-symbols-unprettify-at-point 'right-edge)

;; Limit the visible buffer area to some typographically sound width
;;(require 'big-gutters-mode)
;;(setcq bgm-ignore-mode-regex "dired\\|org-agenda\\|notmuch")
;;(global-big-gutters-mode +1)

;; Highlight FIXME TODO etc. in comments
(use-package fic-mode :init
  ;; This needs to be made globalized
  (fic-mode +1)
  ;; Temporary solution...
  (add-hook 'find-file-hook #'fic-mode)
  :custom
  (fic-highlighted-words (split-string "FIXME TODO BUG XXX")))

;; Highlight the stuff between matching parentheses
;; The idea is to use versor-mode instead of this, but since versor needs
;; work to be good, this is what I'll use for the time being.
(use-package paren
  :config
  (show-paren-mode +1)
  (setcq show-paren-delay 0)
  (setcq show-paren-when-point-inside-paren t)
  (setcq show-paren-style 'expression)
  ;; This is in order for region to take priority over show-paren highlighting
  (setcq show-paren-priority -200))

;; Try to keep the buffer scrolled so the cursor is centered
(require 'simpler-centered-cursor-mode)
(diminish 'simpler-centered-cursor-mode)
(use-package centered-cursor-mode :diminish centered-cursor-mode :config
  (global-centered-cursor-mode +1)

  (defun switch-to-simple-scc ()
    "If this is a large file, switch to simpler-centered-cursor-mode."
    (when (> (buffer-size) 32000)
      (centered-cursor-mode -1)
      (simpler-centered-cursor-mode +1)))
  
  (add-hook 'org-mode-hook #'switch-to-simple-scc))

(use-package popup)

;; Avoid the built-in Emacs window manager and try to use the real one
(use-package frames-only-mode :config
  (frames-only-mode)

  ;; A new frame for each LaTeX refresh gets annoying
  (push
   '(".*Org PDF LaTeX Output.*" .
     (display-buffer-no-window . ((allow-no-window . t))))
   display-buffer-alist))

;;; Interaction
;;;; Navigation and fuzzy finding
(use-package ibuffer :bind
  (("C-x C-b" . ibuffer)))

(use-package ivy :diminish ivy-mode :config
  (setcq ivy-initial-inputs-alist nil)
  (ivy-mode +1))
(use-package counsel :bind
  (("M-x" . counsel-M-x)))

(use-package smex)

(use-package recentf
  :bind ("C-x C-r" . counsel-recentf)
  :custom
  (recentf-max-saved-items 100)
  :init
  (recentf-mode t))

(use-package saveplace
  :custom
  (save-place t)
  (save-place-file (expand-file-name "var/.places" user-emacs-directory)))

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

(require 'versor)
;;(autoload 'versor-setup "versor")
;;(versor-setup 'arrows 'quiet-underlying)


;;(use-package versor
;;  :ensure nil
;;  :init
;;  (require 'versor)
;;:config
;;  (versor-setup
;;   'arrows
;;   'arrows-misc
;;   'meta
;;   'ctrl-x
;;   'text-in-code
;;   'quiet-underlying
;;   'local)

;;  (setcq versor-auto-change-for-modes nil)
;;  (setcq versor-move-out-when-at-end nil)
;;  (setcq versor-level-wrap nil)
;;  (setcq versor-meta-level-wrap nil)
;;  (setcq versor-text-level 2)
;;  (setcq versor-text-meta-level 4)
;;  (setcq versor-non-text-level 2)
;;  (setcq versor-non-text-meta-level 6)
;;
;;  (setcq versor-meta-dimensions-valid-for-modes
;;         '((t t "cartesian" "structural" "text" "structured text" "program")))
;;  
;;  (setcq versor-mode-current-levels
;;         (mapcar #'versor-mode-levels-triplet
;;                 '((emacs-lisp-mode "structural" "exprs")
;;                   (lisp-interaction-mode "structural" "exprs")
;;                   (c-mode "program" "statement-parts")
;;                   (text-mode "text" "words")
;;                   (message-mode "text" "words")
;;                   (org-mode "text" "words"))))
;;
;;  (let ((color (face-attribute 'show-paren-match :background)))
;;    (set-face-attribute 'versor-item-face nil :inherit 'unspecified :background color)
;;    (seq-doseq (metamovemap (seq-subseq moves-moves 1))
;;      (seq-doseq (movemap (seq-subseq metamovemap 1))
;;        (versor-define-move movemap 'color color)
;;        (versor-define-move movemap :background color))))
;;)

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

;;;; Completion with company mode (hopefully practically intrusion-free)
(use-package company :diminish company-mode :init
  (defun company-complete-common-or-selected ()
    "Insert the common part, or if none, complete using selection."
    (interactive)
    (when (company-manual-begin)
      (if (not (equal company-common company-prefix))
          (company--insert-candidate company-common)
        (company-complete-selection))))
  
  (global-company-mode)
  :custom
  (company-frontends '(company-preview-frontend))
  :config
  (unbind-key "<return>" company-active-map)
  (bind-key "TAB" #'company-complete-common-or-selected company-active-map))

;;;; Miscellaneous
;;;;; Export window contents to neat HTML
(use-package htmlize
  :commands (htmlize-buffer htmlize-file htmlize-many-files htmlize-region))

;;;;; Analyse command usage frequency to optimise config
(use-package keyfreq :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))
;;; Editing
(use-package autorevert :config
  (global-auto-revert-mode 1))

(use-package undo-tree :diminish undo-tree-mode :config
  (global-undo-tree-mode)
  (setcq undo-tree-visualizer-diff t))

(use-package smerge-mode :config
  (defun sm-try-smerge ()
    "Start smerge-mode automatically when a git conflict is detected."
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode 1))))

  (add-hook 'find-file-hook 'sm-try-smerge t))

(use-package visual-regexp :config
  (use-package visual-regexp-steroids :bind
    (("C-\%" . #'vr/query-replace)
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
  (("C-z" . whitespace-mode))

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
(unbind-key "<f8>")
(use-package projectile :bind-keymap
  ("<f8>" . projectile-command-map)
  :config
  (projectile-mode)
  (use-package counsel-projectile :config
    (counsel-projectile-mode)))

;;;; Programming, language-specific
(use-package cc-mode :config
  (setcq c-default-style "stroustrup")
  (setcq c-basic-offset 4))

;; Let's hope this is easier to configure (i.e. that I don't have to
;; configure it at all)
(use-package web-mode :mode "\\.html" :custom
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-css-colorization nil))

(use-package intero :config
  (add-hook 'haskell-mode-hook #'intero-mode))

(use-package ada-mode :custom
  (flycheck-gnat-args "-gnat12"))

(use-package ess :init (require 'ess-site))

;;;; Prose
(use-package synosaurus :bind
  (("C-@" . synosaurus-choose-and-replace))
  :config
  (setcq synosaurus-choose-method 'popup)
  (add-hook 'text-mode-hook #'synosaurus-mode))

;;; EAAS = Emacs-As-An-(operating)-System
;;;; Calculator
(use-package calc
  :bind (("<f12>" . calc))
  :custom
  (calc-display-trail nil)
  (calc-simplify-mode 'units))

;;;; File manager
(unbind-key "<f2>")
(use-package sunrise-commander :bind
  (("<f2>" . sunrise-cd))

  :config
  (add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode)))

;;;; Git integration
(use-package magit :bind
  (("<f3>" . magit-status)))

;;;; Organizer, planner, note taking etc.
(use-package calendar :config
  (setcq calendar-date-style 'iso))

(unbind-key "<f4>")
(use-package org
  :ensure org-plus-contrib
  :bind (("<f4> a" . org-agenda)
         ("<f4> i" . capture-general-inbox)
         ("<f4> m" . capture-mail-inbox)
         ("<f4> o" . org-cycle-agenda-files)
         ("<f4> l" . org-store-link)
         ("<f4> RET" . narrow-or-widen-dwim))
  :demand t
  :mode ("\\.org\\'" . org-mode)

  :defines org-capture-templates org-latex-classes

  :init
  (require 'org-notmuch)
  (require 'ox-latex)

  (defun capture-general-inbox (args)
    "Run i capture with ARGS."
    (interactive "P")
    (org-capture args "i"))

  (defun capture-mail-inbox (args)
    "Run m capture with ARGS."
    (interactive "P")
    (org-capture args "m"))

  (defun skip-living-projects ()
    "Skip top level trees that do have a TODO or WAIT child item"
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (case-fold-search nil))
      (and (re-search-forward "TODO\\|WAIT" subtree-end t)
           subtree-end)))

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

  (setcq org-export-backends '(org html publish s5 latex rss))

  ;; Allow longer sections of italics, and italicise mid-word with
  ;; zero width no break space
  (setcq org-emphasis-regexp-components
         '("- ﻿\t('\"{"
           "- ﻿\t.,:!?;'\")}\\["
           " \t\r\n"
           "."
           8))
  
  :custom
  ;;;;;; Regular Org operation
  (org-return-follows-link t)
  (org-list-allow-alphabetical t)
  (org-ellipsis "↴")
  
  (org-show-context-detail
   '((agenda . ancestors)
     (bookmark-jump . lineage)
     (isearch . lineage)
     (default . ancestors)))
  
  ;;;;;; Using Org as a planner
  (org-todo-keywords
   '((sequence "HOLD(h)" "WAIT(w)" "TODO(t)" "|")
     (sequence "|" "DONE(d)")
     (sequence "|" "CANCELED(c)")))

  (org-todo-keyword-faces
   '(("HOLD" . (:foreground "dodger blue" :weight bold))
     ("WAIT" . (:foreground "black" :weight bold))
     ("TODO" . (:foreground "dark orange" :weight bold))
     ("DONE" . (:foreground "olivedrab3" :weight bold))
     ("CANCELED" . (:foreground "dim grey" :weight bold))))

  ;; Normally we'd want tasks to reset to HOLD, but since this is a
  ;; repeated task it also has a new scheduled date so it's okay if it
  ;; becomes a todo because it won't clutter until scheduled anyway!
  (org-todo-repeat-to-state "TODO")

  (org-hierarchical-todo-statistics nil)

  ;; When closing an item, ask for a note – just in case there's an
  ;; important thought there that may otherwise not get recorded
  (org-log-done 'note)
  ;; Don't ask for a log message if cycling through with shift-arrow keys
  (org-treat-S-cursor-todo-selection-as-state-change nil)

  ;; Let's simplify this...
  ;; A = screamingly important
  ;; B = normal day-to-day "you should do this or bad things will happen"
  ;; C = fine if rescheduled
  (org-lowest-priority ?C)
  (org-default-priority ?B)

  ;; Capturing and refiling
  (org-capture-templates
   '(("i" ">inbox" entry (file "") "* %?\n")
     ("m" "mail>inbox" entry (file "") "* %?\n%a\n")))
  (org-default-notes-file "~/org/inbox.org")
  (org-refile-targets
   '(("~/org/projects.org" :maxlevel . 3)
     ("~/org/tickler.org" :maxlevel . 1)
     ("~/org/someday.org" :maxlevel . 3)
     ("~/org/notes.org" :maxlevel . 2)))
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  (org-log-refile 'time)
  (org-reverse-note-order t)

  ;; Agenda and archiving
  (org-agenda-files '("~/org/inbox.org" "~/org/projects.org" "~/org/tickler.org"))
  (org-archive-location "~/org/archive.org::* %s")

  (org-enforce-todo-dependencies t)
  (org-agenda-dim-blocked-tasks 'invisible)
  (org-agenda-span 'day)
  (org-agenda-use-time-grid nil)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)

  (org-agenda-custom-commands
   '((" " "Agenda"
      ((tags "FILE={inbox.org}"
             ((org-agenda-overriding-header "Inbox")))
       (agenda "" nil)
       (tags "-@out/TODO"
             ((org-agenda-overriding-header "To do (not scheduled)")
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
       (todo "WAIT"
             ((org-agenda-overriding-header "Waiting")
              (org-agenda-todo-ignore-scheduled t)))
       (tags "FILE={projects.org}+LEVEL=1-noproject"
             ((org-agenda-overriding-header "Stuck projects")
              (org-agenda-skip-function #'skip-living-projects)))))))

  ;;;;;; Using Org to publish documents
  (org-babel-python-command "python3")
  (org-export-with-smart-quotes t)
  (org-export-with-emphasize t)
  (org-export-with-sub-superscripts nil)
  (org-export-with-footnotes t)

  (org-latex-compiler "xelatex")
  (org-latex-default-class "tufte-handout")
  (org-latex-packages-alist
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
       "\\fi\n")))

  ;;;;;; Supporting code
  :config
  (use-package org-bullets :init
    (add-hook 'org-mode-hook
              (lambda () (org-bullets-mode 1))))

  (org-set-emph-re 'org-emphasis-regexp-components
                   org-emphasis-regexp-components)
  
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t) (R . t) (python . t)))
  
  (push (append '(("tufte-handout"
                   "\\documentclass[a4paper,11pt]{tufte-handout}"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}"))
                  ("tufte-book"
                   "\\documentclass[a4paper,10pt]{tufte-book}")))
        org-latex-classes))


;;;; Email client
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



;;; Diminish a bunch of modes according to the theme
;; We do tihs here because apparently use-package will override it otherwise
(dolist (mode (mapcar #'car modern-minik-mode-icon-alist))
  (unless (member mode '(flycheck-mode))
    (diminish mode (modern-minik-mode-icon mode))))


(provide 'init-common)
;;; init-common.el ends here
