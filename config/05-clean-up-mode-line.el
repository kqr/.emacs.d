
(defun buffer-status-indicator ()
  "Indicate buffer status with letters instead of symbols."
  (if (buffer-local-value buffer-read-only (window-buffer))
      "(read-only) "
    (if (buffer-modified-p)
        "(***) "
      " ")))


(setq-default
 mode-line-format
 (list "%e"
       '(:eval (when (buffer-local-value buffer-read-only (window-buffer))
                 "(read-only)"))
       '(:eval (when (buffer-modified-p)
                 "(***)"))
       " "
       'mode-line-buffer-identification
       ":"
       '(:eval (propertize "%l,%c (~ %P)"))
       " [%["
       '(:eval mode-name)
       " ("
       '(:eval minor-mode-alist)
       " )"
       '(:eval (propertize "%n"))
       "%]]"
       'mode-line-misc-info))
