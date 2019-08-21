(autoload 'magit-status "magit")
(define-key global-map (kbd "<f3>") 'magit-status)
(with-eval-after-load "magit"
  (setq magit-log-margin '(t age magit-log-margin-width t 10))
  (setq magit-diff-refine-hunks 'all)
  (setq magit-diff-arguments '("--stat" "--no-ext-diff" "--diff-algorithm=histogram"))
  (setq magit-log-arguments '("-n64" "--graph" "--decorate" "--color" "--date-order"))

  ;; Restore visibility of foldable sections when opening again
  (setq magit-section-cache-visibility nil))
