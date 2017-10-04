(use-package paredit :diminish paredit-mode :config
  ;; Fixes to make paredit more convenient to work with in other languages
  (setq-default paredit-space-for-delimiter-predicates
                (list (lambda (&rest args) nil)))
  (unbind-key "\\" paredit-mode-map)
  (unbind-key "M-q" paredit-mode-map)
  
  (add-hook 'text-mode-hook #'paredit-mode)
  (add-hook 'prog-mode-hook #'paredit-mode))
