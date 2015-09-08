(require 'package)
(push '("marmalade" . "http://marmalade-repo.org/packages/")
      package-archives )
(push '("melpa" . "http://melpa.milkbox.net/packages/")
      package-archives)
(package-initialize)
(package-install 'use-package)
(eval-when-compile (require 'use-package))

(use-package evil
  :ensure t
  :init (evil-mode 1)
  :config (setq evil-echo-state t))


(menu-bar-mode -1)
(global-linum-mode t)
(setq-default linum-format "%4d ")
(setq make-backup-files nil)

(define-key evil-normal-state-map (kbd ";") #'evil-ex)
(define-key evil-insert-state-map (kbd "TAB") #'evil-normal-state)
(define-key evil-visual-state-map (kbd "TAB") #'evil-normal-state)
(define-key evil-normal-state-map (kbd "<backtab>") #'indent-for-tab-command)
(define-key evil-insert-state-map (kbd "<backtab>") #'indent-for-tab-command)
(define-key evil-visual-state-map (kbd "<backtab>") #'indent-for-tab-command)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe-mode 0 nil (fringe))
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil))
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
 '(mode-line ((t (:background "gray11" :foreground "dim gray"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "black" :foreground "dim grey")))))
