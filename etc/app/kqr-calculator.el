(autoload 'calc "calc")
(define-key global-map (kbd "<f12>") 'calc)
(with-eval-after-load "calc"
  (setq calc-display-trail t
	calc-simplify-mode 'units))
