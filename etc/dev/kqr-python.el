;; Python mode
(with-eval-after-load "python"
  (defun python-mode-configure ()
    (push 'python-mode aggressive-indent-excluded-modes))
  (setq python-shell-interpreter "python3")
  (add-hook 'python-mode-hook 'python-mode-configure))
