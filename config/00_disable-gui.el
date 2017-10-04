
(when (display-graphic-p)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1))

(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(tool-bar-lines . nil))
(menu-bar-mode -1)
(blink-cursor-mode -1)
(global-linum-mode -1)
(setq-default inhibit-startup-screen t)

(require 'diminish)
