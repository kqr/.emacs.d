(autoload 'magit-status "magit")
(define-key global-map (kbd "<f3>") 'magit-status)
(with-eval-after-load "magit"
  (setq magit-log-margin '(t age magit-log-margin-width t 10))
  (setq magit-log-arguments '("-n64" "--graph" "--decorate" "--color" "--date-order")))
