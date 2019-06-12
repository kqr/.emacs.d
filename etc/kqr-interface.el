;; Remove a bunch of distracting, unnecessary, silly graphic components We want
;; to do this very early to avoid annoying flickering of menu bars and such.
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

(if (not (eq system-type 'darwin))
    (define-key global-map (kbd "s-n") 'make-frame)

  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq mac-right-option-modifier 'none))

;; TODO: Figure out what this is supposed to be for non-darwin platforms
(when (eq system-type 'darwin)
  (define-key global-map (kbd "s-<left>") 'ns-prev-frame)
  (define-key global-map (kbd "s-<right>") 'ns-next-frame))

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

