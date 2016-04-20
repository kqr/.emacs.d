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

;; Ace jump for quick & convenient navigation. Similar to easymotion
(use-package ace-jump-mode
  :ensure t)

;; Install and load evil-leader to get a leader key in evil mode. Needs to be
;; loaded before evil itself
(use-package evil-leader
  :ensure t
  :init (global-evil-leader-mode)
  :config (evil-leader/set-leader "SPC"))

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
(use-package evil
  :ensure t
  :init (evil-mode +1)
  :config (progn
    (define-key evil-normal-state-map (kbd "RET") #'open-next-line)
    (define-key evil-normal-state-map (kbd "g t") #'other-window)
    (define-key evil-normal-state-map (kbd ";") #'evil-ex)
    (define-key evil-normal-state-map (kbd "w") #'evil-ace-jump-word-mode)
    (define-key evil-normal-state-map (kbd "h") #'evil-ace-jump-char-mode)
    (define-key evil-insert-state-map (kbd "TAB") #'evil-normal-state)
    (define-key evil-visual-state-map (kbd "TAB") #'evil-normal-state)
    (define-key evil-insert-state-map (kbd "<backtab>") #'indent-for-tab-command)
    (define-key evil-visual-state-map (kbd "<backtab>") #'indent-for-tab-command)
    (define-key evil-insert-state-map (kbd "M-w") (lambda () (insert-char ?å)))
    (define-key evil-insert-state-map (kbd "M-q") (lambda () (insert-char ?ä)))
    (define-key evil-insert-state-map (kbd "M-;") (lambda () (insert-char ?ö)))
    (evil-leader/set-key ";" #'execute-extended-command)
    (evil-leader/set-key "g" #'magit-status)
    (evil-leader/set-key "s" #'toggle-scratch)
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
    (evil-set-initial-state 'magit-blame-mode 'emacs)))

;; surround.vim for evil mode
(use-package evil-surround
  :ensure t
  :init (global-evil-surround-mode +1))

;; fast & convenient fuzzy matching/input completion
(use-package helm
  :ensure t
  :init (progn
          (require 'helm-config)
          (helm-mode +1)))

;; ad hoc project management in emacs. treats any git repo as a project, which
;; makes it easier to generate/search among ctags and open files within that
;; project.
(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :config (progn
            (setq projectile-tags-file-name ".etags")
            (evil-leader/set-key "]" #'projectevil-jump-to-tag)
            (evil-leader/set-key "p" #'helm-projectile-find-file)
            (evil-ex-define-cmd "tn" #'projectevil-tagnext)))

;; helm integration for projectile
(use-package helm-projectile
  :ensure t
  :init (helm-projectile-on))

;; git integration for convenient committing, reviewing diffs, blaming and such
(use-package magit
  :ensure t
  :config (progn
            (setq magit-push-always-verify nil)))

;; org mode!!
(use-package org
  :ensure t)

;; some evil integration with org
(use-package evil-org
  :ensure t
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
                      (evil-insert-state)))))

;; web mode helps with editing files that consist of source code in multiple
;; languages, e.g. html files with embedded javascript and CSS
(use-package web-mode
  :ensure t
  :mode "\\.html?\\'")

;; i prefer to have my cursor centered as much as possible
(use-package centered-cursor-mode
  :ensure t
  :config (global-centered-cursor-mode +1))

;; highlight the current line when you have been idle for a while
(use-package hl-line+
  :ensure t
  :init (progn
          (global-hl-line-mode -1)
          (toggle-hl-line-when-idle +1)))

;; marker at 80 characters. for some reason can't set width other than 1?
(use-package fill-column-indicator
  :ensure t
  :init (progn
    (define-globalized-minor-mode global-fci-mode
      fci-mode (lambda () (fci-mode +1)))
    (global-fci-mode +1))

  :config (progn
    (setq fci-rule-width 1)
    (setq fci-rule-color "grey11")))

;; I do work in Haskell (where I prefer 4 space indents...)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(use-package haskell-mode
  :ensure t
  :config (progn
            (setq haskell-indentation-layout-offset 4)
            (setq haskell-indentation-starter-offset 4)
            (setq haskell-indentation-left-offset 4)
            (setq haskell-indentation-ifte-offset 4)
            (setq haskell-indentation-where-pre-offset 4)
            (setq haskell-indentation-where-post-offset 4)))

;; extension for making emacs understand .less files
(use-package less-css-mode
  :ensure t)

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
 '(hl-line ((t (:background "grey11"))))
 '(linum ((t (:background "grey11" :foreground "dim grey"))))
 '(mode-line ((t (:background "gray11" :foreground "dim gray"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "black" :foreground "dim grey"))))
 '(org-level-1 ((t (:foreground "dodger blue" :weight bold))))
 '(org-level-2 ((t (:foreground "light blue"))))
 '(org-level-3 ((t (:foreground "slate blue"))))
 '(org-link ((t (:foreground "OliveDrab3" :underline t))))
 '(org-todo ((t (:foreground "magenta" :weight bold))))
 '(show-paren-match ((t (:background "dodger blue" :foreground "black"))))
 '(show-paren-mismatch ((t (:background "black" :foreground "red")))))

