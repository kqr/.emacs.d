(when (require 'editorconfig)
  (defun use-reasonable-fill-column-in-text-mode (props)
    (when (derived-mode-p 'text-mode)
      (let ((max-line-length
             (string-to-number (gethash 'max_line_length props "80"))))
        (when (< 120 max-line-length)
          (puthash 'max_line_length "120" props)))))

  (editorconfig-mode 1)

  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode)

  (dolist (override '(use-reasonable-fill-column-in-text-mode))
    (add-hook 'editorconfig-hack-properties-functions
              override)))
