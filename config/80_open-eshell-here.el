(defun eshell-here ()
  "Launch eshell in the same directory as the current buffer."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory)))
    (eshell "new")
    (rename-buffer (concat "*eshell: " parent "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

(bind-key (kbd "C-!") 'eshell-here)
