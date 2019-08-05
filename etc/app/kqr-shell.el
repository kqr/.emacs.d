(when (require 'eshell)
  (require 'em-smart)

  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil)

  (defun get-filename-under-point ()
    "Return a very rough guess at what the filename under point is."
    (interactive)
    (save-excursion
      (let ((filename-end (progn (forward-symbol 1) (point)))
            (filename-beginning (progn (forward-symbol -1) (point))))
        (buffer-substring filename-beginning filename-end))))

  (defun find-file-under-point ()
    "Attempt to open whatever filename is under point."
    (interactive)
    (find-file-other-window (concat default-directory (get-filename-under-point))))

  (defun configure-eshell ()
    (define-key eshell-mode-map (kbd "C-c f") 'eshell/find-file-under-point))

  (add-hook 'eshell-mode-hook 'configure-eshell))
