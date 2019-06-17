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

;;;; Prefer opening frames instead of windows in Emacs
(when (require 'frames-only-mode)
  ;; Trying without frames-only mode for a while
  (frames-only-mode +1)

  ;; Magit maintains a neat stack of buffers and can therefore readily
  ;; replace the current buffer
  (add-to-list 'display-buffer-alist
               '("magit[a-z-]*: .*"
                 (display-buffer-same-window . nil)))

  ;; A new frame for each LaTeX refresh gets annoying
  (add-to-list 'display-buffer-alist
               '(".*Org PDF LaTeX Output.*" .
                 (display-buffer-no-window . ((allow-no-window . t))))))

;; When making new frame, switch it to scratch buffer
(defun switch-to-scratch-buffer (frame)
  "Switch to scratch buffer in FRAME."
  (with-selected-frame frame
    (switch-to-buffer "*scratch*")))

(push 'switch-to-scratch-buffer after-make-frame-functions)


;;;; Window-divider
(setq window-divider-default-bottom-width 5
      window-divider-default-right-width 5)
(window-divider-mode +1)

;;;; Provide this file (to shut up the linter...)
(provide 'kqr-interface)
;;; kqr-interface.el ends here
