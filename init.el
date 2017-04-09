;;;; Dependencies:
;;;; - Emacs 2.24+
;;;; - Git 1.9.4+

;; Enable the built in package manager and install and load the use-package
;; package
(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages/") package-archives)
(push '("melpa" . "http://melpa.milkbox.net/packages/") package-archives)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)


(use-package evil
  ;; Install and load evil mode. Define a bunch of keybinds. Notable binds:
  ;;
  ;;     ; => ":", which means you don't have to press shift to save files
  ;;     TAB => "esc", which means you press tab to escape out of insert mode
  ;;     S-TAB => "TAB", so you can still indent in insert mode
  ;;     <Leader>-; => M-x, for more convenient function execution
  ;;
  ;; Also adds the ex commands
  ;;
  ;;     :k => kill-buffer, which kills the currently active buffer
  ;;     :ko => kill-other-buffers, which knocks out any non-active buffers, to make magit faster
  ;;
  :init (evil-mode +1)
  :config
  (progn
    ; (define-key evil-normal-state-map (kbd "RET") #'open-next-line)
    (define-key evil-normal-state-map (kbd "g t") #'other-window)
    (define-key evil-normal-state-map (kbd ";") #'evil-ex)
    (define-key evil-insert-state-map (kbd "TAB") #'evil-normal-state)
    (define-key evil-visual-state-map (kbd "TAB") #'evil-normal-state)
    (define-key evil-insert-state-map (kbd "<backtab>") #'indent-for-tab-command)
    (define-key evil-visual-state-map (kbd "<backtab>") #'indent-for-tab-command)
    (define-key evil-insert-state-map (kbd "M-w") (lambda () (insert-char ?å)))
    (define-key evil-insert-state-map (kbd "M-q") (lambda () (insert-char ?ä)))
    (define-key evil-insert-state-map (kbd "M-;") (lambda () (insert-char ?ö)))
    (evil-ex-define-cmd "k" #'kill-buffer)
    (evil-ex-define-cmd "ko" #'kill-other-buffers)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
    (setq evil-echo-state t)

    ;; don't use evil mode in git integration windows. it breaks stuff.
    (evil-set-initial-state 'magit-popup-mode 'emacs)
    (evil-set-initial-state 'magit-blame-mode 'emacs)
    (evil-set-initial-state 'notmuch-tree-mode 'emacs)
    (evil-set-initial-state 'gnu-apl-mode 'emacs)
    (evil-set-initial-state 'gnu-apl-interactive-mode 'emacs)

    (use-package evil-surround
      :init (global-evil-surround-mode +1))

    (use-package evil-leader
      :init (global-evil-leader-mode)
      :config
      (progn (evil-leader/set-leader "SPC")
             (evil-leader/set-key ";" #'execute-extended-command)
             (evil-leader/set-key "e" #'eval-expression)
             (evil-leader/set-key "s" #'toggle-scratch)
             (evil-leader/set-key "w" #'switch-to-buffer)
             (evil-leader/set-key "c" #'count-words-region)))))


;;======================================
;; ALWAYS LOADED (related to appearance)
;;======================================

(use-package fic-mode
  :init (add-hook 'prog-mode-hook 'fic-mode))

(use-package centered-cursor-mode
  :config (global-centered-cursor-mode +1))

(use-package hl-line+
  :config
  (progn
    (global-hl-line-mode -1)
    (toggle-hl-line-when-idle +1)))

(use-package fill-column-indicator
  ;; marker at 80 characters. for some reason can't set width other than 1?
  :config
  (progn
    (define-globalized-minor-mode global-fci-mode
      fci-mode (lambda () (fci-mode +1)))
    (global-fci-mode +1)

    (setq fci-rule-width 1)
    (setq fci-rule-color "grey11")))


;;======================================
;; LOADED BASED ON MODE
;;======================================

(use-package less-css-mode
  :mode ("\\.css\\'" "\\.less\\'"))

(use-package ledger-mode
  :mode "\\.journal\\'"
  :config
  (progn
    (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)
    (setq ledger-post-auto-adjust-amounts t)))

(use-package web-mode
  ;; web mode helps with editing files that consist of source code in multiple
  ;; languages, e.g. html files with embedded javascript and CSS
  :mode ("\\.htm\\'" "\\.html\\'")
  :config (setq web-mode-engines-alist '(("django" . "\\.html\\'"))))

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-indentation-mode)
  :config
  (progn
    (setq haskell-indentation-layout-offset 4)
    (setq haskell-indentation-starter-offset 4)
    (setq haskell-indentation-left-offset 4)
    (setq haskell-indentation-ifte-offset 4)
    (setq haskell-indentation-where-pre-offset 4)
    (setq haskell-indentation-where-post-offset 4)))

(use-package org
  :mode "\\.org\\'"
  :config
  (progn
    (setq org-todo-keywords '((sequence "TODO" "|" "DONE" "HOLD")))
    (setq org-todo-keyword-faces '(("HOLD" . (:foreground "dim grey"))))

    ;; some evil integration with org
    (use-package evil-org
      :config (progn
                (evil-define-key 'normal evil-org-mode-map
                  (kbd "RET") #'org-cycle
                  (kbd "x") #'org-ctrl-c-ctrl-c)
                (evil-define-key 'insert org-mode-map
                  (kbd "TAB") #'evil-normal-state
                  (kbd "<tab>") #'evil-normal-state
                  (kbd "<backtab>") #'org-cycle)
                (evil-leader/set-key-for-mode 'org-mode
                  "c" (lambda ()
                        (interactive)
                        (org-table-blank-field)
                        (evil-insert-state)))))))


;;======================================
;; LOADED ON COMMAND
;;======================================
(use-package helm
  ;; fast & convenient fuzzy matching/input completion
  :commands helm-projectile-find-file
  :init
  (progn
    (require 'helm-config)
    (helm-mode +1)))

(use-package expand-region
  :commands er/expand-region
  :config
  (progn
    (setq expand-region-contract-fast-key "z")
    (evil-leader/set-key "x" 'er/expand-region)))

(use-package projectile
  ;; ad hoc project management in emacs. treats any git repo as a project, which
  ;; makes it easier to generate/search among ctags and open files within that
  ;; project.
  :commands (projectevil-jump-to-tag helm-projectile-find-file projectevil-tagnext)
  :init (projectile-global-mode)
  :config
  (progn
    (setq projectile-tags-file-name ".etags")
    (evil-leader/set-key "]" #'projectevil-jump-to-tag)
    (evil-leader/set-key "p" #'helm-projectile-find-file)
    (evil-ex-define-cmd "tn" #'projectevil-tagnext)

    (use-package helm-projectile
      :init (helm-projectile-on))))

(use-package magit
  ;; git integration for convenient committing, reviewing diffs, blaming and such
  :commands magit-status
  :config
  (progn
    (setq magit-push-always-verify nil)
    (evil-leader/set-key "g" #'magit-status)))

(use-package srefactor
  :commands srefactor-refactor-at-point
  :config (progn
            (semantic-mode +1)
            (evil-leader/set-key-for-mode 'c++-mode
              "r" 'srefactor-refactor-at-point)))


(defun em-gnu-apl-init ()
  (setq buffer-face-mode-face 'gnu-apl-default)
  (buffer-face-mode))
(use-package gnu-apl-mode
  :commands gnu-apl
  :config
  (progn
    (add-hook 'gnu-apl-interactive-mode-hook 'em-gnu-apl-init)
    (add-hook 'gnu-apl-mode-hook 'em-gnu-apl-init)))


;; Use whatever version is installed externally, to ensure correct version
;; This means don't get it with use-package!!
(autoload 'notmuch "notmuch" "notmuch mail" t)
(setq message-auto-save-directory "~/mail/drafts")
(setq message-default-mail-headers "Cc: \nBcc: \n")
(setq message-kill-buffer-on-exit t)
(setq mail-specify-envelope-from t)
(setq message-sendmail-envelope-from 'header)
(setq mail-envelope-from 'header)
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/bin/msmtp")

;; Let me insert special characters like åäö even on OS X
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

(setq-default make-backup-files nil)

;; don't word wrap lines, rather behave as vim with horizontal scrolling
(setq-default truncate-lines t)
(set-display-table-slot standard-display-table 0 ?›)
(setq-default fill-column 80)
(setq-default major-mode 'text-mode)

;; by default emacs wants confirmation to open files larger than 10 mb. that's
;; silly.
(setq-default large-file-warning-threshold 100000000)

;; expandtab + four space tabs
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; some indentation settings for cc mode (C, C++, Java etc)
(setq-default c-default-style "stroustrup")
(setq-default c-basic-offset 4)

;; turn off all the gui crap
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode -1)
(setq-default inhibit-startup-screen t)

;; Selecting goes to selection buffer
(setq-default x-select-enable-primary t)

;; line numbers are nice, but I'm trying without for a while
(global-linum-mode -1)
(setq-default linum-format "%4d ")

;; immediately display matching parens
(setq-default show-paren-delay 0)
(show-paren-mode +1)


(defun projectevil-jump-to-tag (arg)
  "find tags file with help from projectile and then evil-jump to the tag
   under point."
  (interactive "P")
  (unless tags-file-name
    (let ((tags-fn projectile-tags-file-name)
          (tags-dir (projectile-project-root)))
      (setq tags-file-name (expand-file-name tags-fn tags-dir))))
  (evil-jump-to-tag arg))

(defun projectevil-tagnext ()
  "jumps to the next tag!"
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'projectevil-jump-to-tag)))

(defun minibuffer-keyboard-quit ()
  "abort recursive edit.
   in delete selection mode, if the mark is active, just deactivate it;
   then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*completions*") (delete-windows-on "*completions*"))
    (abort-recursive-edit)))

(defun open-next-line ()
  "insert a blank line *under* point"
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)))

(defun toggle-scratch ()
  "toggles between scratch buffer and last non-scratch buffer"
  (interactive)
  (if (string-equal (buffer-name) "*scratch*")
      (switch-to-prev-buffer)
      (switch-to-buffer "*scratch*")))

(defun kill-other-buffers ()
  "Kill all buffers that are not the currently active one"
  (interactive)
  (dolist (buf (buffer-list))
    (unless (get-buffer-window buf 'visible) (kill-buffer buf))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "black" :foreground "beige" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 135 :width normal :foundry "nil" :family "Luxi Mono"))))
 '(fic-face ((t (:foreground "red"))))
 '(font-lock-builtin-face ((t nil)))
 '(font-lock-comment-face ((t (:foreground "dim gray"))))
 '(font-lock-constant-face ((t nil)))
 '(font-lock-function-name-face ((t nil)))
 '(font-lock-keyword-face ((t (:foreground "dark orange"))))
 '(font-lock-string-face ((t (:foreground "OliveDrab3"))))
 '(font-lock-type-face ((t (:foreground "dodger blue"))))
 '(font-lock-variable-name-face ((t nil)))
 '(fringe ((t (:background "black" :foreground "grey11"))))
 '(helm-buffer-file ((t (:foreground "dim grey"))))
 '(helm-candidate-number ((t (:foreground "dim grey"))))
 '(helm-ff-file ((t (:foreground "dim grey"))))
 '(helm-match ((t (:foreground "dodger blue"))))
 '(helm-selection ((t (:foreground "dark orange"))))
 '(helm-source-header ((t (:background "grey11" :foreground "white" :weight bold))))
 '(highlight ((t (:background "grey11"))))
 '(hl-line ((t (:background "grey11"))))
 '(ledger-font-payee-pending-face ((t (:foreground "olive drab" :weight normal))))
 '(ledger-font-payee-uncleared-face ((t (:foreground "dim grey"))))
 '(ledger-font-pending-face ((t (:foreground "olivedrab3" :weight normal))))
 '(ledger-font-posting-account-face ((t (:foreground "olivedrab3"))))
 '(ledger-font-posting-amount-face ((t (:foreground "dark orange"))))
 '(ledger-font-posting-date-face ((t (:foreground "dodger blue"))))
 '(linum ((t (:background "grey11" :foreground "dim grey"))))
 '(message-header-cc ((t (:foreground "dim grey"))))
 '(message-header-name ((t (:foreground "dim gray"))))
 '(message-header-other ((t (:foreground "dim grey"))))
 '(message-header-subject ((t (:foreground "default" :weight bold))))
 '(message-header-to ((t (:foreground "dim grey"))))
 '(message-mml ((t (:foreground "dim grey"))))
 '(mode-line ((t (:background "gray11" :foreground "dim gray"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "black" :foreground "dim grey"))))
 '(notmuch-message-summary-face ((t (:foreground "dim grey"))))
 '(notmuch-search-count ((t (:foreground "dim gray"))))
 '(notmuch-search-date ((t (:foreground "dim gray"))))
 '(notmuch-search-matching-authors ((t (:foreground "dark orange"))))
 '(notmuch-search-non-matching-authors ((t (:foreground "dark orange"))))
 '(notmuch-tag-face ((t (:foreground "dodger blue"))))
 '(notmuch-tree-match-author-face ((t (:foreground "dim gray"))))
 '(notmuch-tree-match-date-face ((t (:foreground "dim gray"))) t)
 '(notmuch-tree-match-tag-face ((t (:foreground "dodger blue"))))
 '(notmuch-tree-no-match-author-face ((t (:foreground "dim gray"))) t)
 '(notmuch-tree-no-match-date-face ((t (:foreground "dim gray"))) t)
 '(notmuch-tree-no-match-face ((t nil)))
 '(notmuch-tree-no-match-tag-face ((t (:foreground "dodger blue"))) t)
 '(notmuch-wash-cited-text ((t (:foreground "olivedrab3"))))
 '(org-level-1 ((t (:foreground "dodger blue" :weight bold))))
 '(org-level-2 ((t (:foreground "light blue"))))
 '(org-level-3 ((t (:foreground "slate blue"))))
 '(org-link ((t (:foreground "OliveDrab3" :underline t))))
 '(org-todo ((t (:foreground "magenta" :weight bold))))
 '(show-paren-match ((t (:background "dodger blue" :foreground "black"))))
 '(show-paren-mismatch ((t (:background "black" :foreground "red"))))
 '(widget-field ((t (:background "gray11" :foreground "default")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(message-confirm-send t)
 '(message-hidden-headers
   (quote
    ("^User-Agent:" "^Face:" "^X-Face:" "^X-Draft-From:")))
 '(notmuch-archive-tags (quote ("-inbox" "-unread")))
 '(notmuch-hello-sections
   (quote
    (notmuch-hello-insert-saved-searches notmuch-hello-insert-search notmuch-hello-insert-recent-searches notmuch-hello-insert-alltags notmuch-hello-insert-footer)))
 '(notmuch-poll-script nil)
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i")
     (:name "sent" :query "tag:sent" :key "s")
     (:name "all mail" :query "*" :key "a"))))
 '(notmuch-search-line-faces (quote (("unread" :weight bold))))
 '(notmuch-search-oldest-first nil)
 '(notmuch-show-indent-messages-width 1)
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4))))
 '(send-mail-function (quote smtpmail-send-it)))
