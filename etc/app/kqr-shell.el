(when (require 'eshell)
  (require 'em-smart)

  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil)

  (defun buffer-current-directory ()
    "Get the currently visited directory in this buffer."
    (if (buffer-file-name)
        (file-name-directory (buffer-file-name))
      default-directory))

  (defun buffer-filename-at (point)
    "Return a very rough guess at what the filename under point is."
    (save-excursion
      (when point (goto-char point))
      (let ((filename-end (progn (forward-symbol 1) (point)))
            (filename-beginning (progn (forward-symbol -1) (point))))
        (buffer-substring-no-properties filename-beginning filename-end))))

  (defun buffer-find-file-at (point)
    "Attempt to open whatever filename is under point."
    (interactive "d")
    (find-file-other-window (concat (buffer-current-directory)
                                    (buffer-filename-at nil))))

  (defun buffer-find-file-at-mouse-click (event)
    "Attempt to open whatever filename is clicked in the buffer."
    (interactive "e")
    (buffer-find-file-at (posn-point (event-end event))))

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
    (define-key eshell-mode-map (kbd "C-c f") 'buffer-find-file-at)
    ;; I'm hesitant to enable this since I'm worried it will replace the default
    ;; action. I could, perhaps, advice whatever the default action is, but eh.
    ;; We'll see.
    ;;(define-key eshell-mode-map (kbd "<mouse-1>") 'buffer-find-file-at-mouse-click)

    (with-eval-after-load "evil"
      (evil-define-key 'normal eshell-mode-map (kbd "0") 'eshell-bol)
      (evil-define-key 'normal eshell-mode-map (kbd "I") 'eshell-evil-insert-line)
      (evil-define-key 'normal eshell-mode-map (kbd "RET") 'eshell-send-input)

      ;; Ctrl-D to send EOF in Eshell
      (evil-define-key '(normal insert) eshell-mode-map (kbd "C-d") 'delete-window)))

  (add-hook 'eshell-mode-hook 'configure-eshell))
