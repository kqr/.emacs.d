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
;(setq-default mode-line-format nil)
(global-linum-mode t)
(setq-default linum-format "%4d ")
(setq make-backup-files nil)

(define-key evil-insert-state-map (kbd "TAB") #'evil-normal-state)
(define-key evil-visual-state-map (kbd "TAB") #'evil-normal-state)
(define-key evil-normal-state-map (kbd ";") #'evil-ex)



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
 '(font-lock-variable-name-face ((t nil)))
 '(mode-line ((t (:background "color-235" :foreground "color-242" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "black" :foreground "color-240" :box (:line-width -1 :color "grey75") :weight light)))))
