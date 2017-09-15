;;; kqr-dotemacs --- summary
;;; Commentary:
;;;
;;; * Requires Emacs 25
;;;

;;; Configuration toggles:

;; Comment out an element to disable it from the config.
(defvar *ENABLED*
  '(
    ;; Graphics:
    disable-gui
    instant-show-matching-paren
    highlight-todo-comments
    center-cursor
    highlight-text-beyond-fill-column

    ;; Global binds:
    bind-easy-line-join
    backspace-kills-words

    ;; Config itself:
    config-debugging
    
    ;; God mode:
    cursor-toggles-with-god-mode
    god-mode

    ;; Various Emacs improvements:
    ivy-fuzzy-matching
    undo-tree
    expand-region

    ;; Programming specific minor modes:
    aggressive-indent
    flycheck-with-infer

    ;; Major modes
    org-mode-basic-config
    magit-git-integration
    
    ))

;; TODO:
;; - highlight todo comments
;; - find a way to browse tags (ggtags etc?)
;; - declutter .emacs.d


;;; Various defaults

(setq-default major-mode 'text-mode)

(setq-default x-select-enable-primary t)

(setq-default make-backup-files nil)
(setq-default large-file-warning-threshold 100000000)
(setq-default truncate-lines t)
(set-display-table-slot standard-display-table 0 ?›)
(setq-default fill-column 80)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default c-default-style "stroustrup")
(setq-default c-basic-offset 4)


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


;;; Code:

(defun disable-gui ()
  "Disable various UI elements for a distraction free experience."
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (blink-cursor-mode -1)
  (global-linum-mode -1)
  (setq-default inhibit-startup-screen t)
  (require 'diminish))


(defun instant-show-matching-paren ()
  "Show matching parens immediately."
  (setq-default show-paren-delay 0)
  (show-paren-mode +1))


(defun highlight-todo-comments ()
  "Highlight words like TODO, FIXME and similar in comments."
  (use-package fic-mode :init
    (add-hook 'prog-mode-hook 'fic-mode)))


(defun center-cursor ()
  "Scroll buffers to keep the cursor centered."
  (use-package centered-cursor-mode :config
    (global-centered-cursor-mode +1)))


(defun highlight-text-beyond-fill-column ()
  "Indicate overlong lines by highlighting the protruding text."
  (use-package column-enforce-mode :init
    (global-column-enforce-mode +1)))


(defun bind-easy-line-join ()
  "Set up a quicker keybind to join a line to the one above."
  (global-set-key (kbd "C-J") 'delete-indentation))


(defun unbind-easy-suspend ()
  "Reduce frustration by disabling the ctrl-z bind for suspending Emacs."
  (global-unset-key (kbd "C-z")))


(defun backspace-kills-words ()
  "Make backspace delete entire full words for efficiency."
  (global-set-key (kbd "DEL") 'backward-kill-word))


(defun config-debugging ()
  "Install bug-hunter to better spot errors in dotemacs file."
  (use-package bug-hunter :commands bug-hunter-init-file))


(defun ivy-fuzzy-matching ()
  "Use Ivy for fuzzy matching buffers, files, commands etc."
  (use-package ivy :diminish ivy :config (ivy-mode +1)))


(defun undo-tree ()
  "Enable undo-tree for easier traversal of edit history."
  (use-package undo-tree :diminish undo-tree-mode :config
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-diff t)))


(defun expand-region ()
  "Set up expand-region with convenient key binds."
  (use-package expand-region
    :bind (("C-t" . er/expand-region)
           ("C-o" . contract-or-open-line))
    
    :init
    (defun contract-or-open-line (arg)
      "Open N new lines or contract region, if active."
      (interactive "P")
      (if (region-active-p)
          (er/contract-region arg)
        (open-line arg)))

    :config
    (delete-selection-mode +1)))


(defun aggressive-indent ()
  "Enable aggressive automatic indentation everywhere."
  (use-package aggressive-indent :config
    (global-aggressive-indent-mode +1)))


(defun flycheck-with-infer ()
  "Enable flycheck with Java infer support."
  (use-package flycheck :config
    (global-flycheck-mode 1)
    (add-to-list 'load-path "~/.emacs.d/config/libs")
    (require 'flycheck-infer)))


(defun org-mode-basic-config ()
  "Install org mode with desired keywords."
  (use-package org
    :mode ("\\.org\\'" . org-mode)

    :config
    (setq org-todo-keywords '((sequence "NEW" "TODO" "|" "DONE" "HOLD")))
    (setq org-todo-keyword-faces '(("HOLD" . (:foreground "dim grey"))))))


(defun magit-git-integration ()
  "Install magit and trigger on ctrl-x ctrl-g."
  (use-package magit
    :bind ((("C-x g") . magit-status))))


(defvar god-local-mode)

(defun cursor-toggles-with-god-mode ()
  "Default to bar cursor and switch to box type in God mode."
  (setq-default cursor-type 'box)
  (defun god-mode-update-cursor ()
    (defun god-mode-update-cursor ()
      (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar))))
  (add-hook 'god-mode-enabled-hook 'god-mode-update-cursor)
  (add-hook 'god-mode-disabled-hook 'god-mode-update-cursor))

(defun god-mode ()
  "Reduce wrist pain by installing God mode."
  (use-package god-mode :defines god-mode-isearch-map
    :bind (("<escape>" . god-local-mode)
           :map isearch-mode-map ("<escape>" . god-mode-isearch-activate)
           :map god-mode-isearch-map ("<escape>" . god-mode-isearch-disable))

    :demand t
    
    :init
    (define-key key-translation-map (kbd "<backtab>") (kbd "<tab>"))
    (define-key input-decode-map (kbd "<tab>") (kbd "<escape>"))

    :config
    (god-mode-all)
    (require 'god-mode-isearch)))


(dolist (config *ENABLED*)
  (funcall config))


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
 '(org-agenda-files (quote ("/home/kqr/Dropbox/Orgzly/brain.org")))
 '(package-selected-packages
   (quote
    (undo-tree use-package ivy hl-line+ god-mode flycheck fill-column-indicator fic-mode expand-region column-enforce-mode centered-cursor-mode bug-hunter aggressive-indent)))
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4))))
 '(send-mail-function (quote smtpmail-send-it)))
