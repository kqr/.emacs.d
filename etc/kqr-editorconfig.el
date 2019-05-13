(when (require 'editorconfig)
  (defun use-default-fill-column-in-text-mode (props)
    (when (derived-mode-p 'text-mode)
      (puthash 'max_line_length "80" props)))

  (defun set-olivetti-width-after-editorconfig (props)
    (olivetti-set-width fill-column))

  (editorconfig-mode 1)

  (setq editorconfig-trim-whitespaces-mode 'ws-butler-mode)

  (add-hook 'editorconfig-hack-properties-functions
            'use-default-fill-column-in-text-mode)
  (add-hook 'editorconfig-after-apply-functions
            'set-olivetti-width-after-editorconfig))
