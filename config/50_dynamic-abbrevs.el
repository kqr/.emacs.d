(use-package dabbrev :config
  (bind-key* "C-M-i" 'dabbrev-expand)
  (setq-default dabbrev-case-fold-search nil))
