;;; kqr-interface.el --- Various interface fixes for Emacs.
;;; Commentary:
;;
;; The main purpose of this is to remove many of the distracting graphic
;; components I don't use anyway. Therefore, it might make sense to load this
;; file early to avoid the flickering of said components before this file is
;; loaded.
;;
;;; Code:
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1))
(menu-bar-mode -1)
(blink-cursor-mode -1)

(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(tool-bar-lines . nil))
(add-to-list 'default-frame-alist '(width . 145))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'initial-frame-alist '(left-fringe . 20))
(add-to-list 'default-frame-alist '(left-fringe . 20))
(add-to-list 'initial-frame-alist '(right-fringe . 20))
(add-to-list 'default-frame-alist '(right-fringe . 20))

;; Enable line numbers
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode +1)
  (warn "EMACS VERSION < 26 used. If this is unexpected, look into it."))

;;; Set up some mac-specific interface things
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))

  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'hyper)
  (setq mac-right-command-modifier 'meta)
  (setq mac-right-option-modifier 'none)
  (setq mac-right-alternate-modifier 'none))

;;;; Set up platform-independent shortcuts
(define-key global-map [(hyper w)] 'delete-frame)
(define-key global-map [(hyper n)] 'make-frame)

;;;; Set up platform-specific shortcuts
(when (eq system-type 'darwin)
  ;; Set up CUA bindings on OS X. I use these mostly when not in Evil mode, in
  ;; order not to have to context switch between the Vi-style and Emacs-style
  ;; copy/paste bindings, which almost seem to be picked to cause confusion...
  (define-key global-map [(hyper x)] 'kill-region)
  (define-key global-map [(hyper c)] 'copy-region-as-kill)
  (define-key global-map [(hyper v)] 'yank))

;;;; Windowing stuff

;; Never split vertically
(setq split-height-threshold nil)
;; But feel free to split horizontally, if there's room
(setq split-width-threshold 160)
;; Don't resize windows just because their content changes
(setq even-window-sizes nil)

(autoload 'ace-window "ace-window")
(define-key global-map (kbd "M-o") 'ace-window)

;; When making new frame, switch it to scratch buffer
(defun switch-to-scratch-buffer (frame)
  "Switch to scratch buffer in FRAME."
  (with-selected-frame frame
    (switch-to-buffer "*scratch*")))

(push 'switch-to-scratch-buffer after-make-frame-functions)

;;;; Restore cursor position when reopening file
(save-place-mode +1)

;;;; Window-divider
(setq window-divider-default-bottom-width 5
      window-divider-default-right-width 5)
(window-divider-mode +1)

(autoload 'counsel-M-x "counsel")
(autoload 'counsel-find-file "counsel")
(define-key global-map (kbd "M-x") #'counsel-M-x)
(define-key global-map (kbd "C-x C-f") #'counsel-find-file)

(with-eval-after-load "ivy"
  (ivy-mode +1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ;; Allow input fragments in arbitrary order
        ivy-re-builders-alist '((t . ivy--regex-ignore-order)))

  (with-eval-after-load "evil"
    (define-key evil-normal-state-map (kbd "/") 'counsel-grep-or-swiper)
    (define-key evil-normal-state-map (kbd "C-p") 'counsel-yank-pop))
  (with-eval-after-load "projectile"
    (define-key projectile-command-map (kbd "f") 'counsel-projectile)
    (define-key projectile-command-map (kbd "s G") 'counsel-git-grep)))

;; Enable quick ways of dragging buffers around
(when (require 'buffer-move)
  (define-key global-map (kbd "<C-S-up>") 'buf-move-up)
  (define-key global-map (kbd "<C-S-down>") 'buf-move-down)
  (define-key global-map (kbd "<C-S-left>") 'buf-move-left)
  (define-key global-map (kbd "<C-S-right>") 'buf-move-right))

(when (require 'sr-speedbar)
  ;; No particular configuration at the moment...
  )

;;;; Provide this file (to shut up the linter...)
(provide 'kqr-interface)
;;; kqr-interface.el ends here
