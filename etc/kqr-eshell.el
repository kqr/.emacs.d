(use-package eshell
  :config
  (require 'em-smart)

  (setq eshell-where-to-jump 'begin
        eshell-review-quick-commands nil)

  (defun buffer-current-directory ()
    "Get the currently visited directory in this buffer."
    (if (buffer-file-name)
        (file-name-directory (buffer-file-name))
      default-directory))

  (defun buffer-find-file-at-point-too-many-options ()
    (let* ((project-files (projectile-current-project-files))
           (files (projectile-select-files project-files)))
      (< 5 (length files))))

  (defun buffer-find-file-at-point ()
    "Attempt to open whatever filename is under point."
    (interactive "d")
    (unless (buffer-find-file-at-point-too-many-options)
      (projectile-find-file-dwim-other-window)))

  (defun buffer-find-file-at-mouse-click (event)
    "Attempt to open whatever filename is clicked in the buffer."
    (interactive "e")
    (goto-char (posn-point (event-end event)))
    (buffer-find-file-at-point))

  (defun eshell-open-current-directory ()
    "Open Eshell navigated to the directory of the current buffer in Eshell."
    (interactive)
    (let* ((parent (buffer-current-directory))
           (width (/ (window-total-width) 2))
           (name (car (last (split-string parent "/" t)))))
      ;; A bit of a hack: open eshell buffer in current window...
      (eshell "new")
      ;; ...jump back to previous location...
      (other-buffer)
      ;; ...then attempt to open a new window with the eshell buffer.
      ;; This annoyingly opens the buffer on the wrong side, but I can't figure
      ;; out what to do about that right now.
      (display-buffer (other-buffer) '(display-buffer-use-some-window
                                       (inhibit-same-window . t)))
      (rename-buffer (concat "*eshell: " name "*"))))

  (defun shell-command-on-buffer (arg)
    "Run COMMAND on buffer contents. With prefix, replace buffer contents."
    (interactive "P")
    (shell-command-on-region
     (point-min) (point-max)
     (read-shell-command "Shell command on buffer: ")
     arg
     arg))

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
    (evil-define-key '(normal) 'global (kbd "!") 'eshell-open-current-directory)

    ;; This overrides a different shell-command function which is much less useful.
    (evil-define-key '(normal) 'global (kbd "M-!") 'shell-command-on-buffer))

  (defun configure-eshell ()
    ;; Configure normal Emacs scrolling in Eshell, instead of keeping cursor centered
    (set (make-local-variable 'maximum-scroll-margin) 0.25)
    (set (make-local-variable 'scroll-margin) 0)

    ;; Turn off modes that make less sense in a shell
    (company-mode -1)

    ;; Set up some custome binds
    (define-key eshell-mode-map (kbd "C-c f") 'buffer-find-file-at)
    (define-key eshell-mode-map (kbd "<mouse-1>") 'buffer-find-file-at-mouse-click)

    (with-eval-after-load "evil"
      (evil-define-key 'normal eshell-mode-map (kbd "0") 'eshell-bol)
      (evil-define-key 'normal eshell-mode-map (kbd "I") 'eshell-evil-insert-line)
      (evil-define-key 'normal eshell-mode-map (kbd "RET") 'eshell-send-input)

      ;; Ctrl-D to send EOF in Eshell
      (evil-define-key '(normal insert) eshell-mode-map (kbd "C-d") 'delete-window)))

  (add-hook 'eshell-mode-hook 'configure-eshell))
