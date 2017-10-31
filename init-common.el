;;; init-common.el --- Configuration common to all my Emacs installations
;;
;;; Commentary:
;;
;;; Code:

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

  (when (display-graphic-p)
    (set-fontset-font "fontset-startup" 'unicode
                      (font-spec :name "Whitman" :size 16.0))
    (set-fontset-font "fontset-default" 'unicode
                      (font-spec :name "Symbola" :size 16.0))
    (add-to-list 'initial-frame-alist '(line-spacing . 1))
    (add-to-list 'default-frame-alist '(line-spacing . 1)))
  
  ;; We like our theme (although it's now become a light-modern-thing...)
  (setcq frame-background-mode 'light)
  (load-theme 'modern-minik)

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
  (bind-key* "C-q" 'bury-buffer)
  ;; But then we also need this...
  (bind-key* "C-S-q" 'quoted-insert)

  ;; Make "join this line to the one above" a bit more convenient to perform
  (bind-key* "C-J" 'delete-indentation))


;; Enable easy troubleshooting of init file
(use-package bug-hunter :commands bug-hunter-init-file)


;; Highlight text extending beyond 80 characters
(use-package column-enforce-mode :diminish column-enforce-mode :config
  ;; inherit fill-column
  (setcq column-enforce-column nil)
  (setcq column-enforce-should-enable-p (lambda () t))
  (global-column-enforce-mode +1))


;; Highlight FIXME TODO etc. in comments
(use-package fic-mode :config
  (fic-mode +1)
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


(use-package cc-mode :config
  (setcq c-default-style "stroustrup")
  (setcq c-basic-offset 4))


(use-package autorevert :config
  ;; Don't automatically reload files from disk without asking
  (global-auto-revert-mode -1))


(use-package aggressive-indent :diminish aggressive-indent-mode :config
  (electric-indent-mode -1)
  (global-aggressive-indent-mode +1))


(use-package whitespace :bind
  ("C-=" . whitespace-mode)

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


;; Ivy is great for fuzzy matching in a bunch of places
(use-package ivy :diminish ivy-mode :config (ivy-mode +1))
(use-package counsel :bind
  (("M-x" . counsel-M-x)))


;; Not using paredit anymore since versor-mode does what it does except better
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


(use-package undo-tree :diminish undo-tree-mode :config
  (global-undo-tree-mode)
  (setcq undo-tree-visualizer-diff t))


(use-package dabbrev :config
  (bind-key* "C-M-i" 'dabbrev-expand)
  (setcq dabbrev-case-fold-search nil))


(use-package flycheck :diminish flycheck-mode :config
  (global-flycheck-mode 1))


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
           doc-view-mode
           magit-popup-mode
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

  (defun god-mode-update-cursor ()
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
  (add-hook 'mode-selection-hook (lambda (_) (god-mode-update-cursor)))
  (add-hook 'buffer-selection-hook (lambda (_) (god-mode-update-cursor)))
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
  :ensure nil :load-path "~/.emacs.d/libs"
  :config
  (require 'tab-as-escape)
  (tab-as-escape-mode +1))


(use-package versor
  :ensure nil :load-path "~/.emacs.d/config/libs/emacs-versor/lisp"
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


(unbind-key "<f2>")
(add-to-list 'package-archives
             '("sunrise" . "http://joseito.republika.pl/sunrise-commander/"))
(use-package sunrise-commander :bind
  (("<f2>" . sunrise-cd))

  :config
  (add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode)))


(use-package magit :bind
  (("<f3>" . magit-status)))


(unbind-key "<f4>")
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(use-package org
  :ensure org-plus-contrib
  :bind (("<f4> a" . org-agenda)
         ("<f4> c" . org-capture)
         ("<f4> o" . org-cycle-agenda-files))
  :demand t
  :mode ("\\.org\\'" . org-mode)

  :defines org-capture-templates

  :init
  (setcq org-export-backends '(org html latex s5 publish))
  
  :config
  (setcq org-todo-keywords
         '((sequence "NEW" "HOLD" "TODO" "|" "DONE" "CANCELED")))
  
  (setcq org-todo-keyword-faces
         '(("NEW" . (:foreground "red" :weight bold))
           ("TODO" . (:foreground "dark orange" :weight bold))
           ("DONE" . (:foreground "olivedrab3" :weight bold))
           ("HOLD" . (:foreground "dodger blue" :weight bold))
           ("CANCELED" . (:foreground "dim grey" :weight bold))))

  (setcq org-todo-repeat-to-state "TODO")
  
  (setcq org-lowest-priority ?F)
  (setcq org-default-priority ?D)

  (setcq org-capture-templates
         '(("n" "NEW" entry (file "") "* NEW [#D] %?\n  SCHEDULED: %t")))

  ;; I must come up with a better way to do this synchronization...
  (setcq org-agenda-files '("~/Dropbox/Orgzly/brain.org"))
  (setcq org-default-notes-file "~/Dropbox/Orgzly/brain.org"))


(use-package notmuch
  ;; Load the locally installed notmuch mode to ensure versions match
  :ensure nil
  :config
  (setcq message-auto-save-directory "~/mail/drafts")
  (setcq message-default-mail-headers "Cc: \nBcc: \n")
  (setcq message-kill-buffer-on-exit t)
  (setcq message-sendmail-envelope-from 'header)
  (setcq message-send-mail-function 'message-send-mail-with-sendmail)
  (setcq message-confirm-send t)
  (setcq message-hidden-headers
         '("^User-Agent:" "^Face:" "^X-Face:" "^X-Draft-From"))

  (setcq mail-specify-envelope-from t)
  (setcq mail-envelope-from 'header)

  (setcq send-mail-function 'smtpmail-send-it)
  (setcq sendmail-program "/usr/bin/msmtp")

  (setcq notmuch-archive-tags '("-inbox" "-unread"))

  (defun notmuch-toggle-deleted-tag (&optional beg end)
    (interactive (notmuch-search-interactive-region))
    (if (member "deleted" (notmuch-search-get-tags))
        (notmuch-search-tag (list "-deleted") beg end)
      (notmuch-search-tag (list "+deleted") beg end)))
  (define-key notmuch-search-mode-map "k" #'notmuch-toggle-deleted-tag)

  (setcq notmuch-hello-sections
         '(notmuch-hello-insert-saved-searches
           notmuch-hello-insert-search
           notmuch-hello-insert-recent-searches
           notmuch-hello-insert-alltags
           notmuch-hello-insert-footer))
  (setcq notmuch-poll-script nil)

  (setcq notmuch-saved-searches
         '((:name "inbox" :query "tag:inbox" :key "i")
           (:name "sent" :query "tag:sent" :key "s")
           (:name "all mail" :query "*" :key "a")))

  (setcq notmuch-search-line-faces '(("unread" :weight bold)))

  (setcq notmuch-search-oldest-first nil)
  (setcq notmuch-show-indent-messages-width 4))


(use-package projectile
  :config
  (projectile-mode)
  (use-package counsel-projectile :config
    (counsel-projectile-on)))

;; We do tihs here because apparently use-package will override it otherwise
(dolist (mode (mapcar #'car modern-minik-mode-icon-alist))
  (unless (member mode '(flycheck-mode))
    (diminish mode (modern-minik-mode-icon mode))))



;;; init-common.el ends here
