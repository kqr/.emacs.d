(when (require 'editorconfig)

  (defun use-reasonable-fill-column-in-text-mode (props)
    (when (derived-mode-p 'text-mode)
      (let ((max-line-length
             (string-to-number (gethash 'max_line_length props "80"))))
        (when (< 120 max-line-length)
          (puthash 'max_line_length "120" props)))))

  (defun apply-indentation-to-all-modes (props)
    (let ((indent-size (string-to-number (gethash 'indent_size props "4"))))
      (message "Setting indent size to %S" indent-size)
      (setq c-basic-offset indent-size
            js-indent-level indent-size
            js2-basic-offset indent-size
            sgml-basic-offset indent-size)))

  (editorconfig-mode 1)

  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode)

  (dolist (override '(use-reasonable-fill-column-in-text-mode
                      apply-indentation-to-all-mode))
    (add-hook 'editorconfig-hack-properties-functions
              override)))
