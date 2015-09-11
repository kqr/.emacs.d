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

;; Ace jump for quick & convenient navigation
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
(use-package evil
  :ensure t
  :init (evil-mode +1)
  :config (progn
    (define-key evil-normal-state-map (kbd "RET") #'open-next-line)
    (define-key evil-normal-state-map (kbd "g t") #'other-window)
    (define-key evil-normal-state-map (kbd ";") #'evil-ex)
    (evil-leader/set-key "w" #'evil-ace-jump-word-mode)
    (define-key evil-insert-state-map (kbd "TAB") #'evil-normal-state)
    (define-key evil-normal-state-map (kbd "TAB") #'evil-normal-state)
    (define-key evil-insert-state-map (kbd "<backtab>") #'indent-for-tab-command)
    (define-key evil-normal-state-map (kbd "<backtab>") #'indent-for-tab-command)
    (evil-leader/set-key ";" #'execute-extended-command)
    (evil-leader/set-key "g" #'magit-status)
    (evil-leader/set-key "s" #'toggle-scratch)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
    (setq evil-echo-state t)

    ;; don't use evil mode in git integration windows. it breaks stuff.
    (evil-set-initial-state 'magit-popup-mode 'emacs)
    (evil-set-initial-state 'magit-blame-mode 'emacs)))

(use-package evil-surround
  :ensure t
  :init (global-evil-surround-mode +1))

;; ad hoc project management in emacs. treats any git repo as a project, which
;; makes it easier to generate/search among ctags and open files within that
;; project.
(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :config (progn
            (setq projectile-tags-file-name ".etags")
            (evil-leader/set-key "]" #'projectevil-jump-to-tag)
            (evil-leader/set-key "p" #'projectile-find-file)))

(use-package magit
  :ensure t
  :config (progn
            (setq magit-push-always-verify nil)))

;; i prefer to have my cursor centered as much as possible
(use-package centered-cursor-mode
  :ensure t
  :config (global-centered-cursor-mode +1))

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

;; I do work in Haskell
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(use-package haskell-mode
  :ensure t)

;; extension for making emacs understand .less files
(use-package less-css-mode
  :ensure t)


(setq-default make-backup-files nil)

;; don't word wrap lines, rather behave as vim with horizontal scrolling
(setq-default truncate-lines t)
(set-display-table-slot standard-display-table 0 ?›)
(setq-default fill-column 80)

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

;; line numbers are nice
(global-linum-mode +1)
(setq-default linum-format "%4d ")

;; immediately display matching parens
(setq-default show-paren-delay 0)
(show-paren-mode +1)


(defun projectevil-jump-to-tag ()
  "find tags file with help from projectile and then evil-jump to the tag
   under point."
  (interactive)
  (let *((tags-fn projectile-tags-file-name)
         (tags-dir (projectile-project-root)))
       (setq tags-file-name (expand-file-name tags-fn tags-dir)))
  (evil-jump-to-tag))

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
 '(hl-line ((t (:background "grey11"))))
 '(linum ((t (:background "grey11" :foreground "dim grey"))))
 '(mode-line ((t (:background "gray11" :foreground "dim gray"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "black" :foreground "dim grey"))))
 '(show-paren-match ((t (:background "dodger blue" :foreground "black"))))
 '(show-paren-mismatch ((t (:background "black" :foreground "red")))))

