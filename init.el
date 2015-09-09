(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages/") package-archives)
(push '("melpa" . "http://melpa.milkbox.net/packages/") package-archives)
(package-initialize)
(package-install 'use-package)
(require 'use-package)

(use-package evil
  :ensure t
  :init (evil-mode 1)
  :config
  
  (define-key evil-normal-state-map (kbd ";") #'evil-ex)
  (define-key evil-insert-state-map (kbd "TAB") #'evil-normal-state)
  (define-key evil-visual-state-map (kbd "TAB") #'evil-normal-state)
  (define-key evil-normal-state-map (kbd "<backtab>") #'indent-for-tab-command)
  (define-key evil-insert-state-map (kbd "<backtab>") #'indent-for-tab-command)
  (define-key evil-visual-state-map (kbd "<backtab>") #'indent-for-tab-command)
  (setq evil-echo-state t))

(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :config
  
  (setq projectile-tags-file-name ".etags")
  (define-key evil-normal-state-map (kbd "C-]")
    (lambda ()
      (interactive)
      (let* ((tags-fn projectile-tags-file-name)
         (tags-dir (projectile-project-root))
         (tags-path (expand-file-name tags-fn tags-dir)))
    (setq tags-file-name tags-path))
      (evil-jump-to-tag))))

(use-package centered-cursor-mode
  :ensure t
  :config (global-centered-cursor-mode t))


(setq-default make-backup-files nil)
(setq-default truncate-lines t)
(setq-default large-file-warning-threshold 100000000)
(setq-default indent-tabs-mode nil)

(menu-bar-mode -1)
(scroll-bar-mode nil)
(tool-bar-mode nil)
(tooltip-mode nil)
(global-linum-mode t)
(setq-default linum-format "%4d ")
(set-display-table-slot standard-display-table 0 ?›)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Luxi Mono" :foundry "b&h" :slant normal :weight normal :height 128 :width normal :background "black" :foreground "beige"))))
 '(font-lock-builtin-face ((t nil)))
 '(font-lock-comment-face ((t (:foreground "dim gray"))))
 '(font-lock-constant-face ((t nil)))
 '(font-lock-function-name-face ((t nil)))
 '(font-lock-keyword-face ((t (:foreground "peru"))))
 '(font-lock-string-face ((t (:foreground "OliveDrab3"))))
 '(font-lock-type-face ((t (:foreground "dodger blue"))))
 '(font-lock-variable-name-face ((t nil)))
 '(fringe ((t (:background "black" :foreground "grey11"))))
 '(hl-line ((t (:background "grey11"))))
 '(mode-line ((t (:background "gray11" :foreground "dim gray"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "black" :foreground "dim grey")))))
