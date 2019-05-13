(autoload 'sunrise-cd "sunrise-commander")
(define-key global-map (kbd "<f2>") 'sunrise-cd)
(with-eval-after-load "sunrise-commander"
  (setq sr-show-file-attributes nil))
