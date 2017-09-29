;;; kqr-dotemacs --- summary
;;; Commentary:
;;
;; * Requires Emacs 25
;; * Requires an internet connection
;;

;;; Todo:
;; - find a way to browse tags (ggtags etc?)
;; - find a way to list the defines in current buffer?
;; - abbrev?
;;
;; Inspiration for further items:
;; - https://oremacs.com/2015/04/16/ivy-mode/
;; - https://github.com/julienfantin/.emacs.d/blob/master/init.el
;; - https://github.com/magnars/.emacs.d/blob/master/init.el
;; - https://github.com/sachac/.emacs.d/blob/gh-pages/Sacha.org
;; - https://github.com/rejeep/emacs/
;; - https://github.com/hlissner/doom-emacs
;; - https://github.com/redguardtoo/emacs.d
;; - https://github.com/defunkt/emacs
;; - https://github.com/lunaryorn/old-emacs-configuration
;; - https://github.com/grettke/home/blob/master/.emacs.el
;; - https://github.com/abo-abo/oremacs
;; - https://github.com/kaushalmodi/.emacs.d/blob/master/init.el
;; - https://github.com/grettke/home
;; - https://github.com/jorgenschaefer/Config/blob/master/emacs.el


;;; Configuration toggles:

;; Comment out an element to disable it from the config.
(defvar *ENABLED*
  '(
    ;; Appearance loaded first to avoid silly allocations and flashing UI.
    disable-gui
    instant-show-matching-paren
    highlight-todo-comments
    center-cursor
    no-soft-wrap
    highlight-text-beyond-fill-column
    use-kqr-dark-theme

    ;; Config debugging loaded early to enable even if rest of init is broken.
    config-debugging
    
    ;; God mode loaded next because I'm practically addicted to it.
    god-mode
    cursor-toggles-with-god-mode
    bind-escape-to-tab
    hold-ctrl-for-common

    ;; Global (or globalised) minor modes providing various Emacs improvements.
    ivy-fuzzy-matching
    undo-tree
    expand-region
    paredit-all-the-things  ;; still not 100% sure about this one...

    ;; Programming specific minor modes.
    aggressive-indent
    flycheck
    c-mode-config

    ;; Useful major modes.
    org-mode-basic-config
    magit-git-integration
    open-eshell-here

    ;; Personal keybindings. Last in evaluation order to override other modes.
    fast-buffer-toggle
    ctrl-equals-for-whitespace-mode
    backspace-kills-words
    bind-easy-line-join
    unbind-easy-suspend

    ;; This is probably not what I want anyway so it's commented out
    ;; browse-with-links
    
    ;; Really only for the email server to begin with...
    load-host-specific
    
    ))


;;; Various defaults

(setq-default major-mode 'text-mode)

(setq-default make-backup-files nil)
(setq-default large-file-warning-threshold 100000000)


;;; Prerequisites:

(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages/") package-archives)
(push '("melpa stable" . "http://stable.melpa.org/packages/") package-archives)
(push '("melpa" . "http://melpa.milkbox.net/packages/") package-archives)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package no-littering :init
  (setq backup-inhibited t)
  (setq auto-save-default nil))


;;; Code:

(defun disable-gui ()
  "Disable various UI elements for a distraction free experience."
  (when (display-graphic-p)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (tooltip-mode -1))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
  (add-to-list 'default-frame-alist '(tool-bar-lines . nil))
  (menu-bar-mode -1)
  (blink-cursor-mode -1)
  (global-linum-mode -1)
  (setq-default inhibit-startup-screen t)
  (require 'diminish))


(defun instant-show-matching-paren ()
  "Show matching parens immediately."
  (setq-default show-paren-delay 0)
  (setq-default show-paren-when-point-inside-paren t)
  (setq-default show-paren-style 'expression)
  ;; This is in order for selected region to take priority over
  ;; show-paren expression style
  (setq-default show-paren-priority -200)
  (show-paren-mode +1))


(defun highlight-todo-comments ()
  "Highlight words like TODO, FIXME and similar in comments."
  (use-package fic-mode :init
    (add-hook 'prog-mode-hook 'fic-mode)))


(defun center-cursor ()
  "Scroll buffers to keep the cursor centered."
  (use-package centered-cursor-mode :diminish centered-cursor-mode :config
    (global-centered-cursor-mode +1)))


(defun no-soft-wrap ()
  "Enable horizontal scrolling for lines extending beyond frame border."
  (setq-default truncate-lines t)
  (set-display-table-slot standard-display-table 0 ?›))


(defun highlight-text-beyond-fill-column ()
  "Indicate overlong lines by highlighting the protruding text."
  (use-package column-enforce-mode :diminish column-enforce-mode :init
    (setq-default fill-column 80)
    (global-column-enforce-mode +1)))


(defun config-debugging ()
  "Install bug-hunter to better spot errors in dotemacs file."
  (use-package bug-hunter :commands bug-hunter-init-file))


(defun ivy-fuzzy-matching ()
  "Use Ivy for fuzzy matching buffers, files, commands etc."
  (use-package ivy :diminish ivy-mode :config (ivy-mode +1)))


(defun undo-tree ()
  "Enable undo-tree for easier traversal of edit history."
  (use-package undo-tree :diminish undo-tree-mode :config
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-diff t)))


(defun expand-region ()
  "Set up expand-region with convenient keybinds."
  (use-package expand-region
    :bind (("C-t" . er/expand-region))
    
    :config
    (setq expand-region-contract-fast-key "n")
    (delete-selection-mode +1)))


(defun aggressive-indent ()
  "Enable aggressive automatic indentation everywhere."
  (use-package aggressive-indent :diminish aggressive-indent-mode :config
    (global-aggressive-indent-mode +1)))


(defun paredit-all-the-things ()
  "Enable paredit everywhere."
  (use-package paredit :diminish paredit-mode :config
    ;; Fixes to make paredit more convenient to work with in other languages
    (setq-default paredit-space-for-delimiter-predicates
		  (list (lambda (&rest args) nil)))
    (unbind-key "\\" paredit-mode-map)
    (unbind-key "M-q" paredit-mode-map)
    
    (add-hook 'text-mode-hook #'paredit-mode)
    (add-hook 'prog-mode-hook #'paredit-mode)))


(defun flycheck ()
  "Enable flycheck."
  (use-package flycheck :config
    (global-flycheck-mode 1)))


(defun dynamic-abbrevs ()
  "Enable 'dabbrev-expand' for basic out-of-the-way completion."
  (use-package dabbrev :config
    (bind-key* "C-M-i" 'dabbrev-expand)
    (setq-default dabbrev-case-fold-search nil)))


(defun god-mode ()
  "Reduce wrist pain by installing God mode."
  (use-package god-mode :defines god-mode-isearch-map
    :bind (("<escape>" . god-local-mode)
           :map isearch-mode-map ("<escape>" . god-mode-isearch-activate)
           :map god-mode-isearch-map ("<escape>" . god-mode-isearch-disable))

    :demand t
    
    :init
    (setq god-exempt-major-modes '(magit-mode magit-status-mode magit-popup-mode org-agenda-mode))
    (setq god-exempt-predicates '(god-exempt-mode-p))

    :config
    (god-mode-all)
    (require 'god-mode-isearch)))


(defun hold-ctrl-for-common ()
  "Let you hold down ctrl for common combinations like ctrl-x b."
  (bind-key "C-x C-0" 'delete-window)
  (bind-key "C-x C-1" 'delete-other-windows)
  (bind-key "C-x C-b" 'switch-to-buffer)
  (bind-key "C-x C-o" 'other-window))


(defun cursor-toggles-with-god-mode ()
  "Default to bar cursor and switch to box type in God mode."
  (setq-default cursor-type 'bar)

  (defun god-mode-update-cursor ()
    (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar))
    (cond
     (god-local-mode
      (progn (set-face-background 'mode-line "olive drab")
	     (set-face-foreground 'mode-line "black")))
     (t
      (progn (set-face-background 'mode-line "default")
	     (set-face-foreground 'mode-line "default")))))
  
  (add-hook 'god-mode-enabled-hook 'god-mode-update-cursor)
  (add-hook 'god-mode-disabled-hook 'god-mode-update-cursor))


(defun bind-escape-to-tab ()
  "Universally bind the tab key to mean escape, and shift-tab to mean tab."

  (defun setup-tab-decode (&optional frame)
    "Sets the decode map to interpret tab as escape."
    (when frame
      (select-frame frame))
    (define-key input-decode-map (kbd "<tab>") (kbd "<escape>"))
    (define-key input-decode-map (kbd "C-i") (kbd "<escape>"))
    (define-key input-decode-map "\e[Z" [backtab]))

  ;; Translation maps work "globally" across all emacs clients
  (define-key key-translation-map (kbd "<backtab>") (kbd "TAB"))
  (define-key key-translation-map (kbd "<S-iso-lefttab>") (kbd "TAB"))
  
  ;; Decode maps need to be set up once per frame
  (setup-tab-decode)
  (add-hook 'server-visit-hook #'setup-tab-decode)
  (add-hook 'tty-setup-hook #'setup-tab-decode)
  (add-hook 'window-setup-hook #'setup-tab-decode)
  (add-hook 'after-make-frame-functions #'setup-tab-decode)
  (add-to-list 'after-make-frame-functions #'setup-tab-decode))


(defun c-mode-config ()
  "Set a few defaults for C mode."
  (setq-default c-default-style "stroustrup")
  (setq-default c-basic-offset 4))


(defun org-mode-basic-config ()
  "Install org mode with desired keywords."
  (use-package org
    :bind (("C-c C-a" . org-agenda)
	   ("C-c C-c" . org-capture)
	   ("M-g" . org-agenda-redo))  ;; More convenient in God mode
    :mode ("\\.org\\'" . org-mode)

    :defines org-capture-templates
    :config
    (setq org-todo-keywords '((sequence "NEW" "TODO" "|" "DONE" "HOLD" "CANCELED")))
    (setq org-todo-keyword-faces
	  '(("NEW" . (:foreground "red" :weight bold))
	    ("TODO" . (:foreground "dark orange" :weight bold))
	    ("DONE" . (:foreground "olivedrab3" :weight bold))
	    ("HOLD" . (:foreground "dodger blue" :weight bold))
	    ("CANCELED" . (:foreground "dim grey" :weight bold))))
    
    (setq org-lowest-priority ?F)
    (setq org-default-priority ?D)

    (setq org-capture-templates
	  '(("n" "NEW" entry (file "") "* NEW [#D] %?\n  SCHEDULED: %t")))
    
    (setq org-agenda-files '("~/Dropbox/Orgzly/brain.org"))
    (setq org-default-notes-file "~/Dropbox/Orgzly/brain.org")))


(defun magit-git-integration ()
  "Install magit and trigger on x g g."
  (use-package magit :defines god-exempt-major-modes
    :bind (("C-x M-g" . magit-status))
    :config
    (add-to-list 'god-exempt-major-modes 'magit-mode)))


(defun open-eshell-here ()
  "Add a keybind to launch eshell in the same directory as the current buffer."

  (defun eshell-here ()
    "Launch eshell in the same directory as the current buffer."
    (interactive)
    (let* ((parent (if (buffer-file-name)
		       (file-name-directory (buffer-file-name))
		     default-directory)))
      (eshell "new")
      (rename-buffer (concat "*eshell: " parent "*"))
      (insert (concat "ls"))
      (eshell-send-input)))

  (bind-key (kbd "C-!") 'eshell-here))


(defun fast-buffer-toggle ()
  "Add a keybind to switch to the previous buffer."

  (defun switch-to-last-buffer ()
    "Switch to the previous buffer."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))
  
  (bind-key* "C-#" 'switch-to-last-buffer))


(defun x11-clipboard-integration ()
  "Enable the X11 clipboard in Emacs."
  (setq-default x-select-enable-primary t))


(defun bind-easy-line-join ()
  "Set up a quicker keybind J to join a line to the one above."
  (bind-key* "C-J" 'delete-indentation))


(defun unbind-easy-suspend ()
  "Reduce frustration by disabling the z binding for suspending Emacs."
  (unbind-key "C-z"))


(defun ctrl-equals-for-whitespace-mode ()
  "Turn whitespace mode on and off by pressing =."

  (defun toggle-showparen-with-whitespace (arg)
    "Ensures show-paren-mode is off when whitespace-mode is turned on."
    (if (bound-and-true-p whitespace-mode)
	(show-paren-mode -1)
      (show-paren-mode +1)))

  (advice-add 'whitespace-mode :after #'toggle-showparen-with-whitespace)

  (bind-key* "C-=" 'whitespace-mode)

  (defvar whitespace-style)
  (setq whitespace-style
	'(face trailing tabs spaces newline space-mark tab-mark newline-mark))
  (defvar whitespace-display-mappings)
  (setq whitespace-display-mappings
	'((space-mark 32 [183] [46])
	  (tab-mark 9 [187 9] [92 9])
	  (newline-mark 10 [182 10]))))


(defun backspace-kills-words ()
  "Make backspace delete entire full words for efficiency."

  (defun kill-word (arg)
    "Do what I mean and don't kill the word if there is whitespace to kill..."
    (interactive "P")
    (kill-region (point) (progn (forward-same-syntax arg) (point))))

  (defun kill-region-or-word-dwim (arg)
    "Either kill the active region or kill a word forward"
    (interactive "P")
    (if (region-active-p)
	(kill-region (region-beginning) (region-end))
      (kill-word arg)))
  
  (bind-key "C-w" 'kill-region-or-word-dwim)
  (bind-key* "DEL" 'backward-kill-word)
  (eval-after-load "paredit" #'(bind-key* "DEL" 'paredit-backward-kill-word)))


(defun edit-init ()
  "Interactive command to open the .emacs init file."
  (interactive)
  (find-file (substitute-in-file-name "$HOME/.emacs.d/init.el")))


(defun use-kqr-dark-theme ()
  "Use the kqr-theme specified by kqr-dark-theme.el."
  (let ((fn (substitute-in-file-name "$HOME/.emacs.d/kqr-dark-theme.el")))
    (when (file-readable-p fn)
      (load fn)
      (apply-kqr-dark-theme))))


(defun browse-with-links ()
  "Set links as the generic browser."
  (setq-default browse-url-browser-function 'browse-url-generic)
  (setq-default browse-url-generic-program "links"))


(defun load-host-specific ()
  "Load any configuration files found under config."
  (let ((fn (substitute-in-file-name "$HOME/.emacs.d/config/*.el")))
    (when (file-readable-p fn)
      (load fn))))



(dolist (config *ENABLED*)
  (funcall config))


;; This is host-specific and as such needs to be moved eventually
(defun notmuch-mode ()
  "Load the locally installed notmuch mode to ensure versions match."
  (autoload 'notmuch "notmuch" "notmuch mail" t)
  (defvar mail-envelope-from)
  (defvar sendmail-program)
  (defvar notmuch-archive-tags)
  (defvar notmuch-hello-sections)
  (defvar notmuch-poll-script)
  (defvar notmuch-saved-searches)
  (defvar notmuch-search-line-faces)
  (defvar notmuch-search-oldest-first)
  (defvar notmuch-show-indent-messages-width)
  
  (setq message-auto-save-directory "~/mail/drafts")
  (setq message-default-mail-headers "Cc: \nBcc: \n")
  (setq message-kill-buffer-on-exit t)
  (setq message-sendmail-envelope-from 'header)
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq message-confirm-send t)
  (setq message-hidden-headers
	'("^User-Agent:" "^Face:" "^X-Face:" "^X-Draft-From"))

  (setq mail-specify-envelope-from t)
  (setq mail-envelope-from 'header)

  (setq send-mail-function 'smtpmail-send-it)
  (setq sendmail-program "/usr/bin/msmtp")

  (setq notmuch-archive-tags '("-inbox" "-unread"))
  (setq notmuch-hello-sections
	'(notmuch-hello-insert-saved-searches
	  notmuch-hello-insert-search
	  notmuch-hello-insert-recent-searches
	  notmuch-hello-insert-alltags
	  notmuch-hello-insert-footer))
  (setq notmuch-poll-script nil)
  (setq notmuch-saved-searches
	'(((:name "inbox" :query "tag:inbox" :key "i")
	   (:name "sent" :query "tag:sent" :key "s")
	   (:name "all mail" :query "*" :key "a"))))
  (setq notmuch-search-line-faces '(("unread" :weight bold)))

  (setq notmuch-search-oldest-first nil)
  (setq notmuch-show-indent-messages-width 4))


