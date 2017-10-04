(defun switch-to-last-buffer ()
  "Switch to the previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(bind-key* "C-#" 'switch-to-last-buffer)
