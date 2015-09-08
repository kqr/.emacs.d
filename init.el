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
    :config (setq evil-echo-state nil))



(menu-bar-mode -1)
(setq-default mode-line-format nil)
(global-linum-mode t)
(setq-default linum-format "%4d ")
(setq make-backup-files nil)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-builtin-face ((t nil)))
 '(font-lock-comment-face ((t (:foreground "color-243"))))
 '(font-lock-constant-face ((t nil)))
 '(font-lock-function-name-face ((t nil)))
 '(font-lock-keyword-face ((t (:foreground "color-208"))))
 '(font-lock-string-face ((t (:foreground "color-106"))))
 '(font-lock-type-face ((t (:foreground "color-31"))))
 '(font-lock-variable-name-face ((t nil))))
