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
           (width (/ (window-total-width) 2))
           (name (car (last (split-string parent "/" t)))))
      (split-window-horizontally (- width))
      (other-window 1)
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*"))))

  ;; Things that only make sense in combination with evil
  (with-eval-after-load "evil"
    (defun eshell-evil-insert-line ()
      "Go to the beginning of the prompt and enter insert mode."
      (interactive)
      (eshell-bol)
      (evil-insert-state 1))

    ;; This makes it very convenient to open an Eshell, but it disables the
    ;; default shell command running bind. If desired, it can probably be
    ;; bound to C-! or something.
    (evil-define-key '(normal) 'global (kbd "!") 'eshell-open-current-directory))

  (defun configure-eshell ()
    ;; Configure normal Emacs scrolling in Eshell, instead of keeping cursor centered
    (set (make-local-variable 'maximum-scroll-margin) 0.25)
    (set (make-local-variable 'scroll-margin) 0)

    ;; Turn off modes that make less sense in a shell
    (company-mode -1)

    ;; Set up some custome binds
    (define-key eshell-mode-map (kbd "C-c f") 'eshell/find-file-under-point)

    (with-eval-after-load "evil"
      (evil-define-key 'normal eshell-mode-map (kbd "0") 'eshell-bol)
      (evil-define-key 'normal eshell-mode-map (kbd "I") 'eshell-evil-insert-line)
      ;; Ctrl-D to send EOF in Eshell
      (evil-define-key '(normal insert) eshell-mode-map (kbd "C-d") 'delete-window)))

  (add-hook 'eshell-mode-hook 'configure-eshell))
