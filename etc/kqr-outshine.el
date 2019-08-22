(autoload 'outshine-minor-mode "outshine")
(add-hook 'outline-minor-mode-hook #'outshine-mode)
(add-hook 'prog-mode-hook #'outline-minor-mode)
(eval-after-load "outshine"
  '(progn
     (define-key outline-minor-mode-map (kbd "M-n")
       #'outshine-narrow-to-subtree)
     (define-key outline-minor-mode-map (kbd "M-h")
       #'widen)

     (setq-default outshine-startup-folded-p t)

     ;; Allow narrowing to subtree even when inside subtree
     (define-advice outshine-narrow-to-subtree
         (:before (&rest args) narrow-to-subtree-when-inside-subtree)
       (unless (outline-on-heading-p t)
         (outline-previous-visible-heading 1)))))
