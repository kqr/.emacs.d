(defun god-mode-update-cursor ()
  "Make sure the modeline and cursor is updated with god mode state."
  (if (or god-local-mode buffer-read-only)
      (progn (setq cursor-type 'box)
             (set-face-background 'mode-line "chocolate2"))
    (progn (setq cursor-type 'bar)
           (mapc #'enable-theme custom-enabled-themes))))

(add-hook 'god-mode-enabled-hook 'god-mode-update-cursor)
(add-hook 'god-mode-disabled-hook 'god-mode-update-cursor)
(add-hook 'read-only-mode-hook 'god-mode-update-cursor)
