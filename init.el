(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages/") package-archives)
(push '("melpa" . "http://melpa.milkbox.net/packages/") package-archives)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package evil-leader
  :ensure t
  :init (global-evil-leader-mode)
  :config (evil-leader/set-leader "SPC"))

(use-package evil
  :ensure t
  :init (evil-mode +1)
  :config (progn
    (define-key evil-normal-state-map (kbd "RET") #'open-line)
    (define-key evil-normal-state-map (kbd "g t") #'other-window)
    (define-key evil-normal-state-map (kbd ";") #'evil-ex)
    (define-key evil-insert-state-map (kbd "TAB") #'evil-normal-state)
    (define-key evil-normal-state-map (kbd "TAB") #'evil-normal-state)
    (define-key evil-insert-state-map (kbd "<backtab>") #'indent-for-tab-command)
    (define-key evil-normal-state-map (kbd "<backtab>") #'indent-for-tab-command)
    (evil-leader/set-key ";" #'execute-extended-command)
    (evil-leader/set-key "g" #'magit-status)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
    (setq evil-echo-state t)
    (evil-set-initial-state 'magit-popup-mode 'emacs)
    (evil-set-initial-state 'magit-blame-mode 'emacs)))

(use-package evil-surround
  :ensure t
  :init (global-evil-surround-mode +1))

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

(use-package centered-cursor-mode
  :ensure t
  :config (global-centered-cursor-mode +1))

(use-package fill-column-indicator
  :ensure t
  :init (progn
    (define-globalized-minor-mode global-fci-mode
      fci-mode (lambda () (fci-mode +1)))
    (global-fci-mode +1))

  :config (progn
    (setq fci-rule-width 1)
    (setq fci-rule-color "grey11")))

(use-package less-css-mode
  :ensure t)

(setq-default make-backup-files nil)

(setq-default truncate-lines t)
(set-display-table-slot standard-display-table 0 ?›)
(setq-default fill-column 80)

(setq-default large-file-warning-threshold 100000000)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode -1)
(setq-default inhibit-startup-screen t)

(global-linum-mode +1)
(setq-default linum-format "%4d ")

(setq-default show-paren-delay 0)
(show-paren-mode +1)


(defun projectevil-jump-to-tag ()
  "Find tags file with help from projectile and then evil-jump to the tag
   under point."
  (interactive)
  (let *((tags-fn projectile-tags-file-name)
         (tags-dir (projectile-project-root)))
       (setq tags-file-name (expand-file-name tags-fn tags-dir)))
  (evil-jump-to-tag))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
   In Delete Selection mode, if the mark is active, just deactivate it;
   then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))


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

