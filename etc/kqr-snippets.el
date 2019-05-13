;; I really like the idea of this, but I'm not sure it works
(setq yas-snippet-dirs '("~/.emacs.d/etc/snippets"))
(when (require 'yasnippet nil)
  (setq yas-indent-line 'fixed)
  ;; This may be causing a lot of lag so let's turn it off instead.
  ;; I rarely use it anyway!
  (yas-global-mode -1))
