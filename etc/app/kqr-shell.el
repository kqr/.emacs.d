(when (require 'eshell)
  (require 'em-smart)

  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil)

  (defun get-current-directory ()
    "Get the currently visited directory in this buffer."
    (if (buffer-file-name)
        (file-name-directory (buffer-file-name))
      default-directory))

  (defun get-filename-under-point ()
    "Return a very rough guess at what the filename under point is."
    (save-excursion
      (let ((filename-end (progn (forward-symbol 1) (point)))
            (filename-beginning (progn (forward-symbol -1) (point))))
        (buffer-substring filename-beginning filename-end))))

  (defun find-file-under-point ()
    "Attempt to open whatever filename is under point."
    (interactive)
    (find-file-other-window (concat (get-current-directory) (get-filename-under-point))))

  (defun eshell-open-current-directory ()
    "Open Eshell navigated to the directory of the current buffer in Eshell."
    (interactive)
    (let* ((parent (get-current-directory))
           (height (/ (window-total-height) 3))
           (name (car (last (split-string parent "/" t)))))
      (split-window-vertically (- height))
      (other-window 1)
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*"))))

  (defun configure-eshell ()
    (company-mode -1)
    (define-key eshell-mode-map (kbd "C-c f") 'eshell/find-file-under-point)
    (with-eval-after-load "evil"
      ;; Ctrl-D to send EOF in Eshell
      (evil-define-key '(normal insert) eshell-mode-map (kbd "C-d") 'delete-window)))

  (add-hook 'eshell-mode-hook 'configure-eshell))
