(autoload 'global-flycheck-mode "flycheck")
(add-hook 'prog-mode-hook 'global-flycheck-mode)
(with-eval-after-load "flycheck"
  (diminish 'flycheck-mode)

  ;; Removed idle-change from below to test whether C# performance gets better
  (setq flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-display-errors-delay 0.1
        flycheck-idle-change-delay 2.0
        flycheck-php-phpcs-executable "/usr/local/bin/phpcs"
        flycheck-phpcs-standard "psr2"
        flycheck-python-pycompile-executable "python3"))

;; Subword-mode allows word motions to TerminateInCamelCasedWords
(add-hook 'prog-mode-hook 'subword-mode)

;; Indicate indentation levels
(autoload 'highlight-indent-guides-mode "highlight-indent-guides")
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(with-eval-after-load "highlight-indent-guides"
  (setq highlight-indent-guides-method 'column)
  (setq highlight-indent-guides-responsive 'stack)
  (setq highlight-indent-guides-auto-odd-face-perc 2)
  (setq highlight-indent-guides-auto-even-face-perc 2)
  (setq highlight-indent-guides-auto-stack-odd-face-perc 4)
  (setq highlight-indent-guides-auto-stack-even-face-perc 4)
  (setq highlight-indent-guides-auto-top-odd-face-perc 7)
  (setq highlight-indent-guides-auto-top-even-face-perc 7))
