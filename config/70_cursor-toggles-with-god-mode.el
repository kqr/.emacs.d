(defun god-mode-update-cursor ()
  "Make sure the modeline and cursor is updated with god mode state."
  (if (or god-local-mode buffer-read-only)
      (setq cursor-type 'box)
    (setq cursor-type 'bar))
  (if god-local-mode
      (set-face-background 'mode-line (face-attribute 'font-lock-type-face :foreground))
    (mapc #'enable-theme custom-enabled-themes)))

(add-hook 'god-mode-enabled-hook 'god-mode-update-cursor)
(add-hook 'god-mode-disabled-hook 'god-mode-update-cursor)
(add-hook 'read-only-mode-hook 'god-mode-update-cursor)
(add-hook 'after-change-major-mode-hook 'god-mode-update-cursor)
(add-hook 'window-configuration-change-hook 'god-mode-update-cursor)

;; this appears to work well for versor, so let's see if it works for us too!
(add-hook 'mode-selection-hook (lambda (_) (god-mode-update-cursor)))
(add-hook 'buffer-selection-hook (lambda (_) (god-mode-update-cursor)))
(add-hook 'find-file-hook 'god-mode-update-cursor)
