(autoload 'global-flycheck-mode "flycheck")
(add-hook 'prog-mode-hook 'global-flycheck-mode)
(with-eval-after-load "flycheck"
  (diminish 'flycheck-mode)
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-display-errors-delay 0.1
        flycheck-idle-change-delay 2.0
        flycheck-php-phpcs-executable "/usr/local/bin/phpcs"
        flycheck-phpcs-standard "psr2"
        flycheck-python-pycompile-executable "python3"))

;;;; Subword-mode allows word motions to TerminateInCamelCasedWords
(add-hook 'prog-mode-hook 'subword-mode)
