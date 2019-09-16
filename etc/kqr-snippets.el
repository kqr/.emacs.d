;; I really like the idea of this, but I'm not sure it works
(setq yas-snippet-dirs '("~/.emacs.d/etc/snippets"))
(when (require 'yasnippet)
  (setq yas-indent-line 'fixed)
  (yas-global-mode +1))

(when (require 'auto-yasnippet)
  ;; Currently no other configuration
  )
