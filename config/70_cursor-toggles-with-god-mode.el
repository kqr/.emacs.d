(setq-default cursor-type 'bar)

(defun god-mode-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar))
  (cond
   (god-local-mode
    (progn (set-face-background 'mode-line "olive drab")
           (set-face-foreground 'mode-line "black")))
   (t
    (progn (set-face-background 'mode-line "default")
           (set-face-foreground 'mode-line "default")))))

(add-hook 'god-mode-enabled-hook 'god-mode-update-cursor)
(add-hook 'god-mode-disabled-hook 'god-mode-update-cursor)
