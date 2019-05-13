(autoload 'ws-butler-mode "ws-butler")
(add-hook 'prog-mode-hook 'ws-butler-mode)
(add-hook 'text-mode-hook 'ws-butler-mode)

(autoload 'whitespace "whitespace-mode")
(define-key global-map (kbd "C-z") #'whitespace-mode)
(eval-after-load "whitespace"
  '(progn
     (setq-default whitespace-style
		   '(face trailing tabs spaces newline space-mark tab-mark newline-mark))
     (setq-default whitespace-display-mappings
		   '((space-mark 32 [183] [46])
		     (tab-mark 9 [187 9] [92 9])
		     (newline-mark 10 [182 10])))))
